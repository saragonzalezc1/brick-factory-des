import simpy
import numpy as np
import random
import pandas as pd
import os

# ============================================================
# ESCENARIO 1 — PRODUCCIÓN INTERCALADA  (v2)
# Cambios respecto a v1:
#   1. El horno se lanza desde el inicio del warm-up para que
#      el sistema llegue al estado estable con el horno activo.
#   2. Todos los contadores estadísticos se RESETEAN al terminar
#      el warm-up: garantiza que ningún KPI acumula datos del
#      período transitorio.
#   3. batches_kpi incluye throughput y producto_final desglosados
#      por referencia (N4, N3, N5), habilitando ANOVA de dos factores.
# ============================================================

MIX_REFERENCIAS = {"N4": 0.45, "N3": 0.10, "N5": 0.45}

TIEMPO_CALENTAMIENTO = 24 * 20
TIEMPO_SIMULACION    = 24 * 312

NUM_REPLICAS    = 10
NUM_BATCHES     = 13
DIAS_POR_BATCH  = TIEMPO_SIMULACION / 24 / NUM_BATCHES   # 24.0 días
HORAS_POR_BATCH = TIEMPO_SIMULACION / NUM_BATCHES         # 576.0 h

SEMILLA_BASE = 42
DIR_SALIDA   = "resultados_batch_means"

REFS = {
    "N4": {
        "bloques_por_ton": 1000/4.5, "cap_estante": 280, "cap_zorro_humedo": 48,
        "cap_lote": 3640, "cap_seccion_horno": 18200, "setup_extrusora_h": 0.0,
        "tasa_defecto_ext": 0.01, "tasa_defecto_secart": 0.02,
        "tasa_defecto_secnat": 0.05, "tasa_defecto_horno": 0.05,
    },
    "N3": {
        "bloques_por_ton": 1000/5.0, "cap_estante": 320, "cap_zorro_humedo": 43,
        "cap_lote": 3276, "cap_seccion_horno": 16380, "setup_extrusora_h": 0.5,
        "tasa_defecto_ext": 0.01, "tasa_defecto_secart": 0.02,
        "tasa_defecto_secnat": 0.05, "tasa_defecto_horno": 0.05,
    },
    "N5": {
        "bloques_por_ton": 1000/6.5, "cap_estante": 240, "cap_zorro_humedo": 33,
        "cap_lote": 2523, "cap_seccion_horno": 12615, "setup_extrusora_h": 0.5,
        "tasa_defecto_ext": 0.01, "tasa_defecto_secart": 0.02,
        "tasa_defecto_secnat": 0.05, "tasa_defecto_horno": 0.05,
    },
}

CANTIDAD_ESTANTES_TOTALES  = 116
CAPACIDAD_SECADOR          = 60
TIEMPO_SECADO              = 30
CAPACIDAD_PATIO_VENTILADOR = 26666
CAPACIDAD_PATIO_NATURAL    = 53332
TIEMPO_SECADO_VENTILADOR   = 180
TIEMPO_SECADO_NATURAL      = 240
CAMARAS_POR_ESTADO         = 5
NUM_CAMARAS                = 20

# ── Distribuciones (sin cambios) ─────────────────────────────────────────────
def seleccionar_referencia():
    return random.choices(list(MIX_REFERENCIAS), weights=list(MIX_REFERENCIAS.values()))[0]

def tiempo_molienda_bimodal():
    v = (np.random.normal(3.464238, 0.8876048) if random.random() < 0.2842329
         else np.random.normal(6.296459, 2.3376494))
    return max(v, 2)

def tiempo_produccion():
    return np.random.gamma(5.844786, 1/1.069074)

def tiempo_endague():
    return np.random.exponential(1090.61688426301) / 3600

def tiempo_deshorne():
    return np.random.exponential(839.79291208413) / 3600

# ── Función auxiliar de batch ─────────────────────────────────────────────────
def batch_activo(t_sim):
    """Índice 0-based del batch activo. None durante el warm-up."""
    t_ef = t_sim - TIEMPO_CALENTAMIENTO
    if t_ef < 0:
        return None
    return min(int(t_ef // HORAS_POR_BATCH), NUM_BATCHES - 1)

# ── Estado de réplica ─────────────────────────────────────────────────────────
class EstadoReplica:
    """Todos los contadores de una réplica. Los estadísticos se
    resetean al fin del warm-up mediante reset_post_warmup()."""

    def __init__(self):
        self._init_contadores()

        # Serie temporal del monitor (muestras cada 1 h — incluye warm-up
        # para que el filtro post-calentamiento funcione en exportar_resultados)
        self.serie_monitor = {
            "tiempo": [], "buffer_molienda": [], "cola_estantes": [],
            "apq_N4": [], "apq_N3": [], "apq_N5": [],
            "secado_art_ocup": [], "patio_ventilador": [], "patio_natural": [],
            "zorros_prod": [], "zorros_horno": [],
            "estantes_disp": [], "estantes_secado": [], "cola_est_count": [],
        }
        self.registro_diario = []

        # ── Acumuladores por batch ────────────────────────────────────────────
        # throughput_N4/N3/N5 y producto_final_N4/N3/N5 son NUEVOS en v2:
        # permiten ANOVA de dos factores (escenario × referencia).
        self.batches_kpi = [
            {
                # Throughput total y por referencia
                "throughput_bloques":    0,
                "throughput_N4":         0,
                "throughput_N3":         0,
                "throughput_N5":         0,
                # Producto final (bloques buenos) por referencia
                "producto_final_N4":     0,
                "producto_final_N3":     0,
                "producto_final_N5":     0,
                # Chamote por referencia (para tasa de rechazo)
                "chamote_N4":            0,
                "chamote_N3":            0,
                "chamote_N5":            0,
                # Defectos por etapa
                "def_ext_insp":    0, "def_ext_def":    0,
                "def_secart_insp": 0, "def_secart_def": 0,
                "def_secnat_insp": 0, "def_secnat_def": 0,
                "def_horno_insp":  0, "def_horno_def":  0,
                # Utilización (delta calculado por proceso_batch_monitor)
                "util_molino_h":        0.0,
                "util_extrusion_h":     0.0,
                "util_setup_h":         0.0,
                "util_op_produccion_h": 0.0,
                "util_op_endague_h":    0.0,
                "util_op_deshorne_h":   0.0,
                "horas_batch":          HORAS_POR_BATCH,
            }
            for _ in range(NUM_BATCHES)
        ]

        self._snap_util_batch = {
            "molino": 0.0, "extrusion": 0.0, "setup": 0.0,
            "op_produccion": 0.0, "op_endague": 0.0, "op_deshorne": 0.0,
        }

    def _init_contadores(self):
        """Inicializa (o reinicia) todos los contadores estadísticos."""
        self.contador_salida     = {"total": 0, "deshornadas_acum": 0,
                                    "chamote": 0, "reproceso_horno": 0}
        self.contador_produccion = {"total": 0, "molienda": 0}
        self.contador_secnat     = {"total": 0, "zorros": 0, "proceso": 0,
                                    "defectuosas": 0, "buenas": 0}
        self.contador_secart     = {"total": 0}

        self.kpi_ref = {
            ref: {
                "molienda": 0, "produccion": 0, "secart": 0, "secnat": 0,
                "producto_final": 0, "chamote": 0, "reproceso": 0,
                "defecto_ext": 0, "defecto_secart": 0,
                "defecto_secnat": 0, "defecto_horno": 0,
                "ciclos_h": [], "estantes_incompletos": 0,
            }
            for ref in REFS
        }

        self.defectos_etapa = {
            "extrusion":         {"inspeccionados": 0, "defectuosos": 0},
            "secado_artificial": {"inspeccionados": 0, "defectuosos": 0},
            "secado_natural":    {"inspeccionados": 0, "defectuosos": 0},
            "horno":             {"inspeccionados": 0, "defectuosos": 0},
        }

        self.ciclos_completados = []
        # timestamps_entrada: se vacía en el reset para que los ciclos medidos
        # correspondan solo a bloques que entran DESPUÉS del warm-up.
        self.timestamps_entrada = {ref: [] for ref in REFS}
        self.ton_molidas_acum   = {"total": 0.0}
        self.util_maquinas      = {"molino": 0.0, "extrusion": 0.0, "setup": 0.0}
        self.util_operarios     = {"produccion": 0.0, "endague": 0.0, "deshorne": 0.0}
        self.estantes_en_secado = {"count": 0}
        self._snap = {"molienda_ton": 0.0, "produccion_und": 0,
                      "deshornadas_und": 0, "producto_final": 0}

    def reset_post_warmup(self):
        """Resetea todos los contadores estadísticos al terminar el warm-up.
        Los recursos SimPy y buffers NO se tocan: conservan el estado del
        sistema para la continuidad de la simulación.
        Se llama desde ejecutar_replica() justo después de env.run(warm-up)."""
        self._init_contadores()
        # El snapshot de utilización del batch_monitor parte de cero
        # porque util_maquinas y util_operarios también se resetearon.
        self._snap_util_batch = {
            "molino": 0.0, "extrusion": 0.0, "setup": 0.0,
            "op_produccion": 0.0, "op_endague": 0.0, "op_deshorne": 0.0,
        }


# ── Monitores ─────────────────────────────────────────────────────────────────
def proceso_monitor(env, estado, intervalo=1.0):
    s = estado.serie_monitor
    while True:
        yield env.timeout(intervalo)
        s["tiempo"].append(env.now)
        s["buffer_molienda"].append(len(estado._bm.items))
        s["cola_estantes"].append(len(estado._ce.items))
        s["apq_N4"].append(estado._apq["N4"].level)
        s["apq_N3"].append(estado._apq["N3"].level)
        s["apq_N5"].append(estado._apq["N5"].level)
        s["secado_art_ocup"].append(estado._sa.count / CAPACIDAD_SECADOR)
        s["patio_ventilador"].append(estado._pv.level / CAPACIDAD_PATIO_VENTILADOR)
        s["patio_natural"].append(estado._pn.level / CAPACIDAD_PATIO_NATURAL)
        s["zorros_prod"].append(estado._zp.level)
        s["zorros_horno"].append(estado._zh.level)
        s["estantes_disp"].append(estado._ed.level)
        s["estantes_secado"].append(estado.estantes_en_secado["count"])
        s["cola_est_count"].append(len(estado._ce.items))


def proceso_monitor_diario(env, estado, intervalo=24.0):
    dia = 0
    while True:
        yield env.timeout(intervalo)
        dia += 1
        ton_act  = estado.ton_molidas_acum["total"]
        prod_act = estado.contador_produccion["total"]
        desh_act = estado.contador_salida["deshornadas_acum"]
        pfin_act = estado.contador_salida["total"]
        sn = estado._snap
        estado.registro_diario.append({
            "dia": dia,
            "toneladas_molidas":       round(ton_act  - sn["molienda_ton"], 2),
            "unidades_produccion":     prod_act - sn["produccion_und"],
            "unidades_deshornadas":    desh_act - sn["deshornadas_und"],
            "unidades_producto_final": pfin_act - sn["producto_final"],
        })
        sn["molienda_ton"]    = ton_act
        sn["produccion_und"]  = prod_act
        sn["deshornadas_und"] = desh_act
        sn["producto_final"]  = pfin_act


# ── Batch monitor ─────────────────────────────────────────────────────────────
def proceso_batch_monitor(env, estado):
    """Cierra cada batch capturando el delta de utilización.
    Se lanza al inicio de la fase efectiva (t = TIEMPO_CALENTAMIENTO)."""
    for b in range(NUM_BATCHES):
        t_fin = TIEMPO_CALENTAMIENTO + (b + 1) * HORAS_POR_BATCH
        yield env.timeout(max(0.0, t_fin - env.now))

        snap = estado._snap_util_batch
        um   = estado.util_maquinas
        uo   = estado.util_operarios
        bk   = estado.batches_kpi[b]

        bk["util_molino_h"]        = um["molino"]     - snap["molino"]
        bk["util_extrusion_h"]     = um["extrusion"]  - snap["extrusion"]
        bk["util_setup_h"]         = um["setup"]       - snap["setup"]
        bk["util_op_produccion_h"] = uo["produccion"] - snap["op_produccion"]
        bk["util_op_endague_h"]    = uo["endague"]    - snap["op_endague"]
        bk["util_op_deshorne_h"]   = uo["deshorne"]   - snap["op_deshorne"]

        snap["molino"]        = um["molino"]
        snap["extrusion"]     = um["extrusion"]
        snap["setup"]         = um["setup"]
        snap["op_produccion"] = uo["produccion"]
        snap["op_endague"]    = uo["endague"]
        snap["op_deshorne"]   = uo["deshorne"]


# ── Procesos operativos (lógica sin cambios) ──────────────────────────────────
def proceso_molienda(env, estado):
    CAPACIDAD_MAX = 14
    while True:
        with estado._mol.request() as req:
            yield req
            t0 = env.now
            fin = t0 + 12
            while env.now < fin:
                tasa = min(tiempo_molienda_bimodal(), CAPACIDAD_MAX)
                if estado._ba.level <= 0:
                    yield env.timeout(0.1)
                    continue
                ton = min(tasa, estado._ba.level)
                yield estado._ba.get(ton)
                estado.ton_molidas_acum["total"] += ton
                ref     = seleccionar_referencia()
                bloques = int(ton * REFS[ref]["bloques_por_ton"])
                for _ in range(bloques):
                    estado.contador_produccion["molienda"] += 1
                    estado.kpi_ref[ref]["molienda"] += 1
                    t_ent = env.now
                    estado.timestamps_entrada[ref].append(t_ent)
                    yield estado._bm.put({"ref": ref, "t_entrada": t_ent})
                yield env.timeout(1)
            estado.util_maquinas["molino"] += env.now - t0
        yield env.timeout(12)


def proceso_produccion(env, estado):
    estante_actual = {"ref": None, "bloques": 0, "id": 0}
    zorro_actual   = {"ref": None, "bloques": 0}
    cnt_estantes = cnt_zorros = 0
    ref_extrusora = None
    LOTE = 5
    T_CARGUE = 5 / 3600

    while True:
        with estado._ext.request() as req:
            yield req
            t0  = env.now
            fin = t0 + 8
            while env.now < fin:
                if len(estado._bm.items) < LOTE:
                    yield env.timeout(0.01)
                    continue
                primer = yield estado._bm.get()
                ref_lote = primer["ref"]
                if ref_extrusora is not None and ref_lote != ref_extrusora:
                    t_setup = REFS[ref_lote]["setup_extrusora_h"]
                    if t_setup > 0:
                        estado.util_maquinas["setup"] += t_setup
                        yield env.timeout(t_setup)
                    if estante_actual["ref"] not in (None, ref_lote) and estante_actual["bloques"] > 0:
                        cnt_estantes += 1
                        estado.kpi_ref[estante_actual["ref"]]["estantes_incompletos"] += 1
                        yield estado._ce.put({"ref": estante_actual["ref"],
                                              "bloques": estante_actual["bloques"],
                                              "id": cnt_estantes})
                        estante_actual = {"ref": None, "bloques": 0, "id": 0}
                    if zorro_actual["ref"] not in (None, ref_lote) and zorro_actual["bloques"] > 0:
                        yield estado._zp.put(1)
                        zorro_actual = {"ref": None, "bloques": 0}
                ref_extrusora = ref_lote
                yield env.timeout(tiempo_produccion() / 10000)
                resto = []
                for _ in range(LOTE - 1):
                    resto.append((yield estado._bm.get()))
                lote = [primer] + resto

                # Defectos extrusión — solo se registra en batch si es fase efectiva
                lote_ok = []
                for bloque in lote:
                    rb = bloque["ref"]
                    estado.defectos_etapa["extrusion"]["inspeccionados"] += 1
                    b_idx = batch_activo(env.now)
                    if b_idx is not None:
                        estado.batches_kpi[b_idx]["def_ext_insp"] += 1
                    if random.random() > REFS[rb]["tasa_defecto_ext"]:
                        lote_ok.append(bloque)
                    else:
                        estado.defectos_etapa["extrusion"]["defectuosos"] += 1
                        estado.kpi_ref[rb]["defecto_ext"] += 1
                        if b_idx is not None:
                            estado.batches_kpi[b_idx]["def_ext_def"] += 1

                with estado._op_prod.request() as req_op:
                    yield req_op
                    t_op = env.now
                    yield env.timeout(T_CARGUE)
                    estado.util_operarios["produccion"] += env.now - t_op
                    for bloque in lote_ok:
                        rb = bloque["ref"]
                        cap_est   = REFS[rb]["cap_estante"]
                        cap_zorro = REFS[rb]["cap_zorro_humedo"]
                        estado.contador_produccion["total"] += 1
                        estado.kpi_ref[rb]["produccion"] += 1
                        if estado._ed.level > 0 or (estante_actual["bloques"] > 0 and estante_actual["ref"] == rb):
                            if estante_actual["ref"] not in (None, rb) and estante_actual["bloques"] > 0:
                                cnt_estantes += 1
                                estado.kpi_ref[estante_actual["ref"]]["estantes_incompletos"] += 1
                                yield estado._ce.put({"ref": estante_actual["ref"],
                                                      "bloques": estante_actual["bloques"],
                                                      "id": cnt_estantes})
                                estante_actual = {"ref": None, "bloques": 0, "id": 0}
                            if estante_actual["bloques"] == 0:
                                yield estado._ed.get(1)
                                estante_actual["ref"] = rb
                            estante_actual["bloques"] += 1
                            if estante_actual["bloques"] >= cap_est:
                                cnt_estantes += 1
                                yield estado._ce.put({"ref": rb, "bloques": estante_actual["bloques"], "id": cnt_estantes})
                                estante_actual = {"ref": None, "bloques": 0, "id": 0}
                        else:
                            if zorro_actual["ref"] not in (None, rb) and zorro_actual["bloques"] > 0:
                                yield estado._zp.put(1)
                                zorro_actual = {"ref": None, "bloques": 0}
                            if zorro_actual["bloques"] == 0:
                                yield estado._zp.get(1)
                                zorro_actual["ref"] = rb
                            zorro_actual["bloques"] += 1
                            if zorro_actual["bloques"] >= cap_zorro:
                                cnt_zorros += 1
                                ref_zorro = zorro_actual["ref"]
                                zorro_actual = {"ref": None, "bloques": 0}
                                if cnt_zorros % 4 == 0:
                                    env.process(proceso_secado_natural(env, estado, ref_zorro))
            estado.util_maquinas["extrusion"] += env.now - t0
        yield env.timeout(16)


def controlador_secado_artificial(env, estado):
    while True:
        estante = yield estado._ce.get()
        env.process(proceso_secado_artificial(env, estado, estante))
        yield env.timeout(0.5)


def proceso_secado_artificial(env, estado, estante):
    ref = estante["ref"]
    n   = estante["bloques"]
    estado.estantes_en_secado["count"] += 1
    with estado._sa.request() as req:
        yield req
        yield env.timeout(TIEMPO_SECADO)
    estado.estantes_en_secado["count"] -= 1
    with estado._op_endague_secnat.request() as req_op:
        yield req_op
        t_op = env.now
        tasa = REFS[ref]["tasa_defecto_secart"]
        estado.defectos_etapa["secado_artificial"]["inspeccionados"] += n
        defect = int(np.random.binomial(n, tasa))
        buenas = n - defect
        estado.defectos_etapa["secado_artificial"]["defectuosos"] += defect
        estado.kpi_ref[ref]["defecto_secart"] += defect
        estado.util_operarios["endague"] += env.now - t_op
        b_idx = batch_activo(env.now)
        if b_idx is not None:
            estado.batches_kpi[b_idx]["def_secart_insp"] += n
            estado.batches_kpi[b_idx]["def_secart_def"]  += defect
        if defect > 0:
            yield estado._ba.put(defect / REFS[ref]["bloques_por_ton"])
        if buenas > 0:
            yield env.timeout(3/60)
            yield estado._apq[ref].put(buenas)
            estado.contador_secart["total"]  += buenas
            estado.kpi_ref[ref]["secart"]    += buenas
    yield estado._ed.put(1)


def proceso_secado_natural(env, estado, ref):
    unidades = REFS[ref]["cap_zorro_humedo"] * 4
    yield env.timeout(5/60)
    if (estado._pv.level + unidades) <= CAPACIDAD_PATIO_VENTILADOR:
        yield estado._pv.put(unidades)
        env.process(secado_lote(env, estado, unidades, TIEMPO_SECADO_VENTILADOR, estado._pv, ref))
    else:
        yield estado._pn.put(unidades)
        env.process(secado_lote(env, estado, unidades, TIEMPO_SECADO_NATURAL, estado._pn, ref))
    yield env.timeout(5/60)
    yield estado._zp.put(4)
    estado.contador_secnat["zorros"] += 1


def secado_lote(env, estado, unidades, t_secado, patio, ref):
    yield env.timeout(t_secado)
    yield patio.get(unidades)
    with estado._mvh.request() as turno:
        yield turno
        yield estado._zh.get(4)
        with estado._op_endague_secnat.request() as req:
            yield req
            t_op = env.now
            tasa  = REFS[ref]["tasa_defecto_secnat"]
            estado.defectos_etapa["secado_natural"]["inspeccionados"] += unidades
            defect = int(np.random.binomial(unidades, tasa))
            buenas = unidades - defect
            estado.defectos_etapa["secado_natural"]["defectuosos"] += defect
            estado.kpi_ref[ref]["defecto_secnat"] += defect
            estado.contador_secnat["proceso"]     += unidades
            estado.contador_secnat["defectuosas"] += defect
            estado.contador_secnat["buenas"]      += buenas
            b_idx = batch_activo(env.now)
            if b_idx is not None:
                estado.batches_kpi[b_idx]["def_secnat_insp"] += unidades
                estado.batches_kpi[b_idx]["def_secnat_def"]  += defect
            if defect > 0:
                yield estado._ba.put(defect / REFS[ref]["bloques_por_ton"])
            estado.util_operarios["endague"] += env.now - t_op
            yield env.timeout(10/60)
        if buenas > 0:
            yield estado._apq[ref].put(buenas)
            estado.contador_secnat["total"] += buenas
            estado.kpi_ref[ref]["secnat"]   += buenas
            yield env.timeout(5/60)
        yield estado._zh.put(4)


ORDEN_ESTADOS = ["endague", "quema", "coccion", "deshorne"]

def elegir_almacen_con_lote(env, estado):
    while True:
        for ref, alm in estado._apq.items():
            if alm.level >= REFS[ref]["cap_lote"]:
                return ref, alm
        yield env.timeout(0.5)


def proceso_camara(env, estado, estado_inicial, offset_inicial):
    yield env.timeout(offset_inicial)
    idx          = ORDEN_ESTADOS.index(estado_inicial)
    ref_camara   = "N4"
    cap_lote_cam = REFS["N4"]["cap_lote"]

    while True:
        est = ORDEN_ESTADOS[idx % 4]

        if est == "endague":
            ref_camara, alm = yield env.process(elegir_almacen_con_lote(env, estado))
            cap_lote_cam    = REFS[ref_camara]["cap_lote"]
            yield alm.get(cap_lote_cam)
            t0 = env.now
            with estado._op_endague.request() as req:
                yield req
                t_op = env.now
                yield env.timeout(tiempo_endague())
                estado.util_operarios["endague"] += env.now - t_op
            if (env.now - t0) < 24:
                yield env.timeout(24 - (env.now - t0))

        elif est in ("quema", "coccion"):
            yield env.timeout(24)

        elif est == "deshorne":
            t0 = env.now
            with estado._op_deshorne.request() as req:
                yield req
                t_op = env.now
                yield env.timeout(tiempo_deshorne())
                estado.util_operarios["deshorne"] += env.now - t_op

            buenas = reproceso = chamote = 0
            tasa = REFS[ref_camara]["tasa_defecto_horno"]
            estado.defectos_etapa["horno"]["inspeccionados"] += cap_lote_cam

            # Determinar batch — None durante warm-up: acumula en contadores
            # globales pero NO en batches_kpi
            b_idx = batch_activo(env.now)
            if b_idx is not None:
                estado.batches_kpi[b_idx]["def_horno_insp"] += cap_lote_cam

            for _ in range(cap_lote_cam):
                if random.random() < tasa:
                    estado.defectos_etapa["horno"]["defectuosos"] += 1
                    estado.kpi_ref[ref_camara]["defecto_horno"]   += 1
                    if b_idx is not None:
                        estado.batches_kpi[b_idx]["def_horno_def"] += 1
                    if random.random() < 0.5:
                        chamote   += 1
                    else:
                        reproceso += 1
                else:
                    buenas += 1

            if reproceso > 0:
                yield estado._ba.put(reproceso / REFS[ref_camara]["bloques_por_ton"])

            # Contadores globales (ya reseteados al fin del warm-up)
            estado.contador_salida["total"]           += buenas
            estado.contador_salida["chamote"]         += chamote
            estado.contador_salida["reproceso_horno"] += reproceso
            estado.contador_salida["deshornadas_acum"] += buenas + chamote + reproceso
            estado.kpi_ref[ref_camara]["producto_final"] += buenas
            estado.kpi_ref[ref_camara]["chamote"]        += chamote
            estado.kpi_ref[ref_camara]["reproceso"]      += reproceso

            # ── NUEVO v2: throughput y producto_final por referencia por batch ──
            if b_idx is not None:
                bk = estado.batches_kpi[b_idx]
                bk["throughput_bloques"]              += buenas
                bk[f"throughput_{ref_camara}"]        += buenas
                bk[f"producto_final_{ref_camara}"]    += buenas
                bk[f"chamote_{ref_camara}"]           += chamote

            # Tiempo de ciclo — solo estadístico global (no por batch)
            n_sal = buenas + chamote + reproceso
            ts    = estado.timestamps_entrada[ref_camara]
            for _ in range(min(n_sal, len(ts))):
                estado.ciclos_completados.append(env.now - ts.pop(0))
                estado.kpi_ref[ref_camara]["ciclos_h"].append(
                    estado.ciclos_completados[-1])

            if (env.now - t0) < 24:
                yield env.timeout(24 - (env.now - t0))

        idx += 1


def proceso_horno(env, estado):
    for i in range(NUM_CAMARAS):
        env.process(proceso_camara(env, estado,
                                   ORDEN_ESTADOS[i % 4],
                                   (i // 4) * (24 / CAMARAS_POR_ESTADO)))
    yield env.timeout(0)


# ── Exportación ───────────────────────────────────────────────────────────────
def exportar_resultados(escenario, replica, estado, rows_batches,
                        rows_globales, rows_autocorr):
    dias_sim = TIEMPO_SIMULACION / 24
    tiempos  = np.array(estado.serie_monitor["tiempo"])

    for b_idx in range(NUM_BATCHES):
        bk    = estado.batches_kpi[b_idx]
        t_ini = TIEMPO_CALENTAMIENTO + b_idx * HORAS_POR_BATCH
        t_fin = TIEMPO_CALENTAMIENTO + (b_idx + 1) * HORAS_POR_BATCH
        mask  = (tiempos >= t_ini) & (tiempos < t_fin)

        def mb(k):
            a = np.array(estado.serie_monitor[k])
            return float(a[mask].mean()) if mask.any() else float("nan")

        def tasa(d, i): return d/i if i > 0 else float("nan")

        # Tasas de defecto
        td_ext    = tasa(bk["def_ext_def"],    bk["def_ext_insp"])
        td_secart = tasa(bk["def_secart_def"], bk["def_secart_insp"])
        td_secnat = tasa(bk["def_secnat_def"], bk["def_secnat_insp"])
        td_horno  = tasa(bk["def_horno_def"],  bk["def_horno_insp"])

        # Utilizaciones
        util_mol = bk["util_molino_h"]        / (HORAS_POR_BATCH * 1)
        util_ext = bk["util_extrusion_h"]     / (HORAS_POR_BATCH * 1)
        util_prd = bk["util_op_produccion_h"] / (HORAS_POR_BATCH * 5)
        util_end = bk["util_op_endague_h"]    / (HORAS_POR_BATCH * 6)
        util_des = bk["util_op_deshorne_h"]   / (HORAS_POR_BATCH * 4)

        kpis = {
            # ── Throughput total ──────────────────────────────────────────────
            "throughput_bloques":             bk["throughput_bloques"],
            "throughput_bloques_por_dia":     bk["throughput_bloques"] / DIAS_POR_BATCH,
            "throughput_bloques_por_hora":    bk["throughput_bloques"] / HORAS_POR_BATCH,
            # ── Throughput por referencia (NUEVO v2) ─────────────────────────
            "throughput_N4_por_hora":         bk["throughput_N4"] / HORAS_POR_BATCH,
            "throughput_N3_por_hora":         bk["throughput_N3"] / HORAS_POR_BATCH,
            "throughput_N5_por_hora":         bk["throughput_N5"] / HORAS_POR_BATCH,
            # ── Producto final por referencia (NUEVO v2) ─────────────────────
            "producto_final_N4_por_hora":     bk["producto_final_N4"] / HORAS_POR_BATCH,
            "producto_final_N3_por_hora":     bk["producto_final_N3"] / HORAS_POR_BATCH,
            "producto_final_N5_por_hora":     bk["producto_final_N5"] / HORAS_POR_BATCH,
            # ── Tasas de defecto ─────────────────────────────────────────────
            "tasa_defecto_extrusion":         td_ext,
            "tasa_defecto_secado_artificial": td_secart,
            "tasa_defecto_secado_natural":    td_secnat,
            "tasa_defecto_horno":             td_horno,
            # ── Niveles de buffers ───────────────────────────────────────────
            "nivel_buffer_molienda":          mb("buffer_molienda"),
            "nivel_cola_estantes":            mb("cola_estantes"),
            "nivel_almacen_preq_N4":          mb("apq_N4"),
            "nivel_almacen_preq_N3":          mb("apq_N3"),
            "nivel_almacen_preq_N5":          mb("apq_N5"),
            # ── Ocupación de recursos ────────────────────────────────────────
            "ocupacion_secador_artificial":   mb("secado_art_ocup"),
            "ocupacion_patio_ventilador":     mb("patio_ventilador"),
            "ocupacion_patio_natural":        mb("patio_natural"),
            # ── Utilización de máquinas y operarios ──────────────────────────
            "utilizacion_molino":             util_mol,
            "utilizacion_extrusora":          util_ext,
            "utilizacion_op_produccion":      util_prd,
            "utilizacion_op_endague":         util_end,
            "utilizacion_op_deshorne":        util_des,
        }

        for kpi, valor in kpis.items():
            rows_batches.append({
                "escenario": escenario, "replica": replica,
                "batch": b_idx + 1, "t_ini_h": t_ini, "t_fin_h": t_fin,
                "kpi": kpi, "valor": valor,
            })

    # ── Estadísticos globales por réplica ─────────────────────────────────────
    cs = estado.contador_salida
    ac = np.array(estado.ciclos_completados)

    def safe_stat(f, arr): return float(f(arr)) if len(arr) else float("nan")

    globales = {
        "throughput_total_bloques":  cs["total"],
        "throughput_dia_global":     cs["total"] / dias_sim,
        "throughput_hora_global":    cs["total"] / TIEMPO_SIMULACION,
        # Por referencia — global por réplica
        "producto_final_N4":         estado.kpi_ref["N4"]["producto_final"],
        "producto_final_N3":         estado.kpi_ref["N3"]["producto_final"],
        "producto_final_N5":         estado.kpi_ref["N5"]["producto_final"],
        "throughput_dia_N4":         estado.kpi_ref["N4"]["producto_final"] / dias_sim,
        "throughput_dia_N3":         estado.kpi_ref["N3"]["producto_final"] / dias_sim,
        "throughput_dia_N5":         estado.kpi_ref["N5"]["producto_final"] / dias_sim,
        # Tasas de defecto globales
        "tasa_def_extrusion_global": (estado.defectos_etapa["extrusion"]["defectuosos"] /
                                       max(estado.defectos_etapa["extrusion"]["inspeccionados"], 1)),
        "tasa_def_secart_global":    (estado.defectos_etapa["secado_artificial"]["defectuosos"] /
                                       max(estado.defectos_etapa["secado_artificial"]["inspeccionados"], 1)),
        "tasa_def_secnat_global":    (estado.defectos_etapa["secado_natural"]["defectuosos"] /
                                       max(estado.defectos_etapa["secado_natural"]["inspeccionados"], 1)),
        "tasa_def_horno_global":     (estado.defectos_etapa["horno"]["defectuosos"] /
                                       max(estado.defectos_etapa["horno"]["inspeccionados"], 1)),
        # Tiempo de ciclo
        "ciclos_n":        len(ac),
        "ciclo_media_h":   safe_stat(np.mean,   ac),
        "ciclo_mediana_h": safe_stat(np.median, ac),
        "ciclo_min_h":     safe_stat(np.min,    ac),
        "ciclo_max_h":     safe_stat(np.max,    ac),
        "ciclo_p5_h":      safe_stat(lambda a: np.percentile(a,  5), ac),
        "ciclo_p95_h":     safe_stat(lambda a: np.percentile(a, 95), ac),
    }

    for kpi, valor in globales.items():
        rows_globales.append({"escenario": escenario, "replica": replica,
                               "kpi": kpi, "valor": valor})

    # ── Autocorrelación lag-1 ─────────────────────────────────────────────────
    kpis_ac = [
        "throughput_bloques", "throughput_N4_por_hora", "throughput_N3_por_hora",
        "throughput_N5_por_hora", "tasa_defecto_extrusion", "tasa_defecto_horno",
        "nivel_buffer_molienda", "nivel_cola_estantes",
        "utilizacion_molino", "utilizacion_extrusora",
        "utilizacion_op_produccion", "utilizacion_op_endague", "utilizacion_op_deshorne",
        "ocupacion_secador_artificial", "ocupacion_patio_ventilador", "ocupacion_patio_natural",
    ]
    rep_rows = [r for r in rows_batches
                if r["escenario"] == escenario and r["replica"] == replica]
    for kn in kpis_ac:
        serie = np.array([r["valor"] for r in rep_rows if r["kpi"] == kn], dtype=float)
        serie = serie[~np.isnan(serie)]
        lag1  = float(np.corrcoef(serie[:-1], serie[1:])[0,1]) if len(serie) >= 3 and serie.std() > 0 else float("nan")
        rows_autocorr.append({
            "escenario": escenario, "replica": replica, "kpi": kn,
            "num_batches": NUM_BATCHES, "autocorr_lag1": lag1,
            "advertencia": "REVISAR" if not np.isnan(lag1) and abs(lag1) > 0.3 else "OK",
        })


# ── Ejecución de réplica ──────────────────────────────────────────────────────
def ejecutar_replica(replica_id, verbose=True):
    semilla = SEMILLA_BASE + replica_id
    random.seed(semilla)
    np.random.seed(semilla)
    if verbose:
        print(f"  [E1] Réplica {replica_id:02d} | semilla={semilla}", end=" ", flush=True)

    env    = simpy.Environment()
    estado = EstadoReplica()

    estado._mol = simpy.Resource(env, capacity=1)
    estado._ext = simpy.Resource(env, capacity=1)
    estado._sa  = simpy.Resource(env, capacity=CAPACIDAD_SECADOR)
    estado._pv  = simpy.Container(env, capacity=CAPACIDAD_PATIO_VENTILADOR, init=0)
    estado._pn  = simpy.Container(env, capacity=CAPACIDAD_PATIO_NATURAL,    init=0)
    estado._bm  = simpy.Store(env)
    estado._ce  = simpy.Store(env)
    estado._bmr = simpy.Store(env)
    estado._zp  = simpy.Container(env, init=6, capacity=6)
    estado._zh  = simpy.Container(env, init=7, capacity=7)
    estado._mvh = simpy.Resource(env, capacity=1)
    estado._ba  = simpy.Container(env, init=100000, capacity=1000000)
    estado._apq = {ref: simpy.Container(env, init=0, capacity=10_000_000) for ref in REFS}
    estado._ed  = simpy.Container(env, init=CANTIDAD_ESTANTES_TOTALES,
                                   capacity=CANTIDAD_ESTANTES_TOTALES)
    estado._op_prod           = simpy.Resource(env, capacity=5)
    estado._op_endague        = simpy.Resource(env, capacity=3)
    estado._op_endague_secnat = simpy.Resource(env, capacity=3)
    estado._op_deshorne       = simpy.Resource(env, capacity=4)

    # ── CAMBIO v2: horno se lanza desde el inicio del warm-up ────────────────
    # Esto permite que el horno alcance estado estable junto con el resto
    # del sistema antes de que empiece la recolección de datos.
    env.process(proceso_molienda(env, estado))
    env.process(proceso_produccion(env, estado))
    env.process(controlador_secado_artificial(env, estado))
    env.process(proceso_horno(env, estado))          # ← antes estaba después del warm-up
    env.process(proceso_monitor(env, estado, intervalo=1.0))
    env.process(proceso_monitor_diario(env, estado, intervalo=24.0))

    # Fase 1: Warm-up — el sistema evoluciona con todos los procesos activos
    env.run(until=TIEMPO_CALENTAMIENTO)

    # ── CAMBIO v2: resetear contadores al terminar el warm-up ─────────────────
    # Los recursos y buffers conservan su estado (estado estable alcanzado).
    # Solo se limpian los acumuladores estadísticos.
    estado.reset_post_warmup()

    # Lanzar batch monitor al inicio de la fase efectiva
    env.process(proceso_batch_monitor(env, estado))

    # Fase 2: Simulación efectiva — todos los KPIs se recogen desde aquí
    env.run(until=TIEMPO_CALENTAMIENTO + TIEMPO_SIMULACION)

    if verbose:
        print(f"→ horno={estado.contador_salida['total']:,} bloques")
    return estado


# ── Verificación de coherencia ────────────────────────────────────────────────
def verificar_coherencia(estado, replica_id):
    tm   = estado.contador_produccion["molienda"]
    tb   = estado.contador_salida["total"]
    ch   = estado.contador_salida["chamote"]
    de   = estado.defectos_etapa["extrusion"]["defectuosos"]
    dsa  = estado.defectos_etapa["secado_artificial"]["defectuosos"]
    dsn  = estado.defectos_etapa["secado_natural"]["defectuosos"]
    cont = tb + ch + de + dsa + dsn
    dif  = tm - cont
    ok_b = abs(dif) / max(tm, 1) < 0.15
    zv   = estado._zp.level + estado._zh.level
    et   = estado._ed.level + estado.estantes_en_secado["count"] + len(estado._ce.items)
    print(f"  [Coherencia R{replica_id:02d}] Mol={tm:,} Cont={cont:,} WIP={dif:,} "
          f"Balance={'✓' if ok_b else '⚠'} Zorros={zv}{'✓' if zv<=13 else '⚠'} "
          f"Estantes={et}{'✓' if et==CANTIDAD_ESTANTES_TOTALES else '⚠'}")


# ── Bloque principal ──────────────────────────────────────────────────────────
if __name__ == "__main__":
    os.makedirs(DIR_SALIDA, exist_ok=True)
    print("=" * 65)
    print("  ESCENARIO 1 — PRODUCCIÓN INTERCALADA  (v2)")
    print(f"  {NUM_REPLICAS} réplicas × {NUM_BATCHES} batches × {DIAS_POR_BATCH:.0f} días/batch")
    print(f"  Warm-up: {TIEMPO_CALENTAMIENTO/24:.0f} días | Sim efectiva: {TIEMPO_SIMULACION/24:.0f} días")
    print(f"  Horno activo desde t=0 | Contadores reseteados al fin del warm-up")
    print("=" * 65)

    rows_batches = []; rows_globales = []; rows_autocorr = []

    for r in range(1, NUM_REPLICAS + 1):
        est = ejecutar_replica(r, verbose=True)
        verificar_coherencia(est, r)
        exportar_resultados("E1_intercalado", r, est,
                            rows_batches, rows_globales, rows_autocorr)


    ruta_b = os.path.join(DIR_SALIDA, "E1_batches.csv")
    ruta_g = os.path.join(DIR_SALIDA, "E1_globales.csv")
    ruta_a = os.path.join(DIR_SALIDA, "E1_autocorrelacion.csv")

    pd.DataFrame(rows_batches).to_csv(ruta_b,  index=False)
    pd.DataFrame(rows_globales).to_csv(ruta_g, index=False)
    pd.DataFrame(rows_autocorr).to_csv(ruta_a, index=False)


    df_ac = pd.DataFrame(rows_autocorr)
    rev   = df_ac[df_ac["advertencia"] == "REVISAR"]
    if not rev.empty:
        print("\n  ⚠  Autocorrelación lag-1 > 0.3:")
        for k in rev["kpi"].unique():
            print(f"     {k}: media={rev[rev['kpi']==k]['autocorr_lag1'].mean():.3f}")
    else:
        print("\n  ✓  Todas las autocorrelaciones lag-1 ≤ 0.3")
    print("=" * 65)
