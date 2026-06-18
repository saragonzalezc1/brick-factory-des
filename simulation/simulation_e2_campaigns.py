import simpy
import numpy as np
import random
import pandas as pd
import os

# ============================================================
# ESCENARIO 2 — PRODUCCIÓN POR CAMPAÑAS  (v2)
# Las referencias se producen en bloques de tiempo (campañas)
# que se repiten cíclicamente: N4 → N5 → N3 → N4 → ...
# ============================================================

# ============================================================
# --- PARÁMETROS CONFIGURABLES DEL ESCENARIO ---
# ============================================================

# Duración de cada campaña en horas (corregido según especificación)
DURACION_CAMPANA = {
    "N4": 48,   # horas produciendo N4
    "N5": 48,   # horas produciendo N5
    "N3": 12,   # horas produciendo N3
}
# Orden de rotación de campañas
ORDEN_CAMPANAS = ["N4", "N5", "N3"]

TIEMPO_CALENTAMIENTO = 24 * 20    # 20 días de warm-up (igual que E1)
TIEMPO_SIMULACION    = 24 * 312   # 312 días de simulación efectiva

# ============================================================
# --- PARÁMETROS BATCH MEANS (NUEVO) ---
#
# NOTA DE ALINEACIÓN CICLO-BATCH:
# El ciclo completo de campaña es N4(48h) + N5(48h) + N3(12h) = 108h.
# Para evitar sesgo sistemático, el tamaño de batch debe ser
# múltiplo entero de 108h.
#   - 108h × 1  =  4.5 días   → 69.3 batches en 312 días (no entero)
#   - 108h × 4  =  18 días    → 17.3 batches (no entero)
#   - 108h × 8  =  36 días    → 8.67 batches (no entero)
#   - 108h × 14 = 63 días     → 4.95 batches (no entero)
#
# El mínimo común múltiplo de 108h y 24h es 216h (9 días).
# Con 312 días = 7488h:
#   - 216h × 34 = 7344h  (34 batches × 9 días = 306 días) → sobra 6 días
#   - 216h × 35 = 7560h  → excede 312 días
#
# La opción que minimiza el sesgo manteniendo batches completos y
# cobertura total de 312 días es:
#   12 batches × 648h (27 días) = 324 días > 312 → ligero padding al final
#
#   *** SOLUCIÓN ADOPTADA: 12 batches × 648h (27 días) ***
#   Cada batch contiene exactamente 6 ciclos completos de campaña
#   (6 × 108h = 648h). Esto elimina el sesgo por fracción de ciclo.
#   Los últimos 12 días del horizonte (312−27×12=−12 → padding de
#   324−312=12 días) quedan en el último batch, que tendrá menos
#   datos pero no sesgo sistemático.
#
# Si se prefiere 13 × 24 días como en E1, cambiar los parámetros:
#   NUM_BATCHES   = 13
#   HORAS_POR_BATCH = 576.0
# y aceptar el sesgo de fracción de ciclo (5.33 ciclos/batch).
# ============================================================
NUM_REPLICAS    = 10
NUM_BATCHES     = 12       # 12 batches × 27 días = 324h → cubre 312 días con padding
HORAS_POR_BATCH = 648.0    # 27 días × 24h = 648h = exactamente 6 ciclos de campaña
DIAS_POR_BATCH  = HORAS_POR_BATCH / 24   # 27.0 días

# Semillas: iguales a E1 para comparación con varianza reducida
SEMILLA_BASE = 42

DIR_SALIDA = "resultados_batch_means"

# ============================================================
# --- PARÁMETROS POR REFERENCIA ---
# ============================================================
REFS = {
    "N4": {
        "bloques_por_ton":     1000 / 4.5,
        "cap_estante":         280,
        "cap_zorro_humedo":    48,
        "cap_lote":            3640,
        "cap_seccion_horno":   18200,
        "tasa_defecto_ext":    0.01,
        "tasa_defecto_secart": 0.02,
        "tasa_defecto_secnat": 0.05,
        "tasa_defecto_horno":  0.05,
        "setup_extrusora_h":   0.0,
    },
    "N3": {
        "bloques_por_ton":     1000 / 5.0,
        "cap_estante":         320,
        "cap_zorro_humedo":    43,
        "cap_lote":            3276,
        "cap_seccion_horno":   16380,
        "tasa_defecto_ext":    0.01,
        "tasa_defecto_secart": 0.02,
        "tasa_defecto_secnat": 0.05,
        "tasa_defecto_horno":  0.05,
        "setup_extrusora_h":   0.5,
    },
    "N5": {
        "bloques_por_ton":     1000 / 6.5,
        "cap_estante":         240,
        "cap_zorro_humedo":    33,
        "cap_lote":            2523,
        "cap_seccion_horno":   12615,
        "tasa_defecto_ext":    0.01,
        "tasa_defecto_secart": 0.02,
        "tasa_defecto_secnat": 0.05,
        "tasa_defecto_horno":  0.05,
        "setup_extrusora_h":   0.5,
    },
}

# ============================================================
# --- PARÁMETROS DEL SISTEMA ---
# ============================================================
CANTIDAD_ESTANTES_TOTALES  = 116
CAPACIDAD_SECADOR          = 60
TIEMPO_SECADO              = 30
CAPACIDAD_PATIO_VENTILADOR = 26666
CAPACIDAD_PATIO_NATURAL    = 53332
TIEMPO_SECADO_VENTILADOR   = 180
TIEMPO_SECADO_NATURAL      = 240
CAMARAS_POR_ESTADO         = 5
NUM_CAMARAS                = 20

# ============================================================
# --- FUNCIONES DE DISTRIBUCIÓN ---
# ============================================================
def tiempo_molienda_bimodal():
    p = random.random()
    valor = np.random.normal(3.464238, 0.8876048) if p < 0.2842329 else np.random.normal(6.296459, 2.3376494)
    return max(valor, 2)

def tiempo_produccion():
    return np.random.gamma(5.844786, 1 / 1.069074)

def tiempo_endague():
    return np.random.exponential(1090.61688426301) / 3600

def tiempo_deshorne():
    return np.random.exponential(839.79291208413) / 3600

# ============================================================
# --- FUNCIÓN AUXILIAR: batch activo ---
# ============================================================
def batch_activo(t_sim, t_warmup, horas_por_batch):
    """Calcula el batch (0-based) al que pertenece t_sim.
    Retorna None durante el warm-up."""
    t_efectivo = t_sim - t_warmup
    if t_efectivo < 0:
        return None
    b = int(t_efectivo // horas_por_batch)
    return min(b, NUM_BATCHES - 1)

# ============================================================
# --- CLASE ESTADO DE RÉPLICA (NUEVO) ---
# ============================================================
class EstadoReplica:
    """Contenedor de todos los contadores mutables de una réplica."""
    def __init__(self):
        self.contador_salida     = {"total": 0, "deshornadas_acum": 0, "chamote": 0, "reproceso_horno": 0}
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
                "ciclos_h": [],
                "campanas": 0,           # número de campañas completadas
                "horas_campana": 0.0,    # horas totales en campaña
            }
            for ref in REFS
        }

        self.defectos_etapa = {
            "extrusion":         {"inspeccionados": 0, "defectuosos": 0},
            "secado_artificial": {"inspeccionados": 0, "defectuosos": 0},
            "secado_natural":    {"inspeccionados": 0, "defectuosos": 0},
            "horno":             {"inspeccionados": 0, "defectuosos": 0},
        }

        # Historial de campañas para análisis de E2
        self.historial_campanas = []   # {ref, t_inicio, t_fin, dur_h}

        self.ciclos_completados = []
        self.util_maquinas      = {"molino": 0.0, "extrusion": 0.0, "setup": 0.0}
        self.util_operarios     = {"produccion": 0.0, "endague": 0.0, "deshorne": 0.0}
        self.estantes_en_secado = {"count": 0}
        self.timestamps_entrada = {ref: [] for ref in REFS}
        self.ton_molidas_acum   = {"total": 0.0}
        self._snap = {"molienda_ton": 0.0, "produccion_und": 0,
                      "deshornadas_und": 0, "producto_final": 0}

        # Campaña activa (compartida entre procesos de esta réplica)
        self.ref_campana_actual = {"ref": ORDEN_CAMPANAS[0]}

        # Serie temporal del monitor
        self.serie_monitor = {
            "tiempo": [], "buffer_molienda": [], "cola_estantes": [],
            "apq_N4": [], "apq_N3": [], "apq_N5": [],
            "secado_art_ocup": [], "patio_ventilador": [], "patio_natural": [],
            "zorros_prod": [], "zorros_horno": [],
            "estantes_disp": [], "estantes_secado": [], "cola_est_count": [],
            "campana_activa": [],
        }

        # Acumuladores por batch (núcleo de Batch Means)
        self.batches_kpi = [
            {
                "throughput_bloques":       0,
                # Throughput y producto final por referencia (NUEVO v2)
                "throughput_N4":            0,
                "throughput_N3":            0,
                "throughput_N5":            0,
                "producto_final_N4":        0,
                "producto_final_N3":        0,
                "producto_final_N5":        0,
                "chamote_N4":               0,
                "chamote_N3":               0,
                "chamote_N5":               0,
                "def_ext_insp":             0, "def_ext_def":            0,
                "def_secart_insp":          0, "def_secart_def":         0,
                "def_secnat_insp":          0, "def_secnat_def":         0,
                "def_horno_insp":           0, "def_horno_def":          0,
                "util_molino_h":            0.0,
                "util_extrusion_h":         0.0,
                "util_setup_h":             0.0,
                "util_op_produccion_h":     0.0,
                "util_op_endague_h":        0.0,
                "util_op_deshorne_h":       0.0,
                "horas_batch":              HORAS_POR_BATCH,
                # NUEVO en E2: horas de campaña activa dentro del batch por referencia
                "horas_campana_N4":         0.0,
                "horas_campana_N3":         0.0,
                "horas_campana_N5":         0.0,
            }
            for _ in range(NUM_BATCHES)
        ]

        self._snap_util_batch = {
            "molino": 0.0, "extrusion": 0.0, "setup": 0.0,
            "op_produccion": 0.0, "op_endague": 0.0, "op_deshorne": 0.0,
        }

        self.registro_diario = []

    def _init_contadores(self):
        """Reinicia todos los acumuladores estadísticos."""
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
                "ciclos_h": [], "campanas": 0, "horas_campana": 0.0,
            }
            for ref in REFS
        }
        self.defectos_etapa = {
            "extrusion":         {"inspeccionados": 0, "defectuosos": 0},
            "secado_artificial": {"inspeccionados": 0, "defectuosos": 0},
            "secado_natural":    {"inspeccionados": 0, "defectuosos": 0},
            "horno":             {"inspeccionados": 0, "defectuosos": 0},
        }
        self.ciclos_completados  = []
        self.timestamps_entrada  = {ref: [] for ref in REFS}
        self.ton_molidas_acum    = {"total": 0.0}
        self.util_maquinas       = {"molino": 0.0, "extrusion": 0.0, "setup": 0.0}
        self.util_operarios      = {"produccion": 0.0, "endague": 0.0, "deshorne": 0.0}
        self.historial_campanas  = []
        self._snap = {"molienda_ton": 0.0, "produccion_und": 0,
                      "deshornadas_und": 0, "producto_final": 0}

    def reset_post_warmup(self):
        """Resetea contadores estadísticos al fin del warm-up.
        Los recursos SimPy conservan su estado (estado estable alcanzado)."""
        self._init_contadores()
        self._snap_util_batch = {
            "molino": 0.0, "extrusion": 0.0, "setup": 0.0,
            "op_produccion": 0.0, "op_endague": 0.0, "op_deshorne": 0.0,
        }


# ============================================================
# --- PROCESO CONTROLADOR DE CAMPAÑAS (lógica original E2) ---
# Actualiza ref_campana_actual y registra historial de campañas.
# ============================================================
def proceso_campanas(env, estado):
    """Controla la rotación de campañas de producción.
    Cicla por ORDEN_CAMPANAS con las duraciones de DURACION_CAMPANA."""
    idx = 0
    while True:
        ref = ORDEN_CAMPANAS[idx % len(ORDEN_CAMPANAS)]
        dur = DURACION_CAMPANA[ref]
        t_inicio = env.now

        estado.ref_campana_actual["ref"] = ref
        estado.kpi_ref[ref]["campanas"]      += 1
        estado.kpi_ref[ref]["horas_campana"] += dur

        yield env.timeout(dur)

        estado.historial_campanas.append({
            "ref": ref, "t_inicio": t_inicio,
            "t_fin": env.now, "dur_h": dur,
        })
        idx += 1

# ============================================================
# --- MONITORES ---
# ============================================================
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
        s["campana_activa"].append(estado.ref_campana_actual["ref"])

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
            "campana_activa":          estado.ref_campana_actual["ref"],
            "toneladas_molidas":       round(ton_act  - sn["molienda_ton"], 2),
            "unidades_produccion":     prod_act - sn["produccion_und"],
            "unidades_deshornadas":    desh_act - sn["deshornadas_und"],
            "unidades_producto_final": pfin_act - sn["producto_final"],
        })
        sn["molienda_ton"]    = ton_act
        sn["produccion_und"]  = prod_act
        sn["deshornadas_und"] = desh_act
        sn["producto_final"]  = pfin_act

# ============================================================
# --- PROCESO BATCH MONITOR (NUEVO) ---
# ============================================================
def proceso_batch_monitor(env, estado):
    """Cierra batches al fin de cada ventana temporal y captura
    deltas de utilización. También registra horas de campaña
    activa por referencia dentro de cada batch."""
    for b in range(NUM_BATCHES):
        t_ini_batch = TIEMPO_CALENTAMIENTO + b * HORAS_POR_BATCH
        t_fin_batch = TIEMPO_CALENTAMIENTO + (b + 1) * HORAS_POR_BATCH
        yield env.timeout(max(0, t_fin_batch - env.now))

        snap = estado._snap_util_batch
        um   = estado.util_maquinas
        uo   = estado.util_operarios
        bk   = estado.batches_kpi[b]

        bk["util_molino_h"]        = um["molino"]     - snap["molino"]
        bk["util_extrusion_h"]     = um["extrusion"]  - snap["extrusion"]
        bk["util_setup_h"]         = um["setup"]       - snap["setup"]
        bk["util_op_produccion_h"] = uo["produccion"]  - snap["op_produccion"]
        bk["util_op_endague_h"]    = uo["endague"]     - snap["op_endague"]
        bk["util_op_deshorne_h"]   = uo["deshorne"]    - snap["op_deshorne"]

        snap["molino"]        = um["molino"]
        snap["extrusion"]     = um["extrusion"]
        snap["setup"]         = um["setup"]
        snap["op_produccion"] = uo["produccion"]
        snap["op_endague"]    = uo["endague"]
        snap["op_deshorne"]   = uo["deshorne"]

        # Calcular horas de campaña activa por referencia dentro del batch
        # usando el historial de campañas completadas + la campaña en curso
        tiempos_serie = np.array(estado.serie_monitor["tiempo"])
        camp_serie    = np.array(estado.serie_monitor["campana_activa"])
        mask_b = (tiempos_serie >= t_ini_batch) & (tiempos_serie < t_fin_batch)
        if mask_b.any():
            camp_en_batch = camp_serie[mask_b]
            for ref in REFS:
                bk[f"horas_campana_{ref}"] = float(np.sum(camp_en_batch == ref))

# ============================================================
# --- PROCESO MOLIENDA ---
# MODIFICADO vs original E2: lee ref_campana_actual del estado.
# ============================================================
def proceso_molienda(env, estado):
    bm  = estado._bm
    ba  = estado._ba
    mol = estado._mol
    CAPACIDAD_MAX = 14
    while True:
        with mol.request() as req:
            yield req
            t_inicio_uso = env.now
            fin_jornada  = t_inicio_uso + 12

            while env.now < fin_jornada:
                tasa = min(tiempo_molienda_bimodal(), CAPACIDAD_MAX)
                if ba.level <= 0:
                    yield env.timeout(0.1)
                    continue

                ton_procesadas = min(tasa, ba.level)
                yield ba.get(ton_procesadas)
                estado.ton_molidas_acum["total"] += ton_procesadas

                ref     = estado.ref_campana_actual["ref"]
                bloques = int(ton_procesadas * REFS[ref]["bloques_por_ton"])
                for _ in range(bloques):
                    estado.contador_produccion["molienda"] += 1
                    estado.kpi_ref[ref]["molienda"] += 1
                    t_ent = env.now
                    estado.timestamps_entrada[ref].append(t_ent)
                    yield bm.put({"ref": ref, "t_entrada": t_ent})

                yield env.timeout(1)

            estado.util_maquinas["molino"] += env.now - t_inicio_uso
        yield env.timeout(12)

# ============================================================
# --- PROCESO PRODUCCIÓN ---
# ============================================================
def proceso_produccion(env, estado):
    bm  = estado._bm
    ext = estado._ext
    ce  = estado._ce
    ed  = estado._ed
    zp  = estado._zp
    op  = estado._op_prod
    apq = estado._apq
    pv  = estado._pv
    pn  = estado._pn
    ba  = estado._ba

    estante_actual    = {"ref": None, "bloques": 0, "id": 0}
    zorro_actual      = {"ref": None, "bloques": 0}
    contador_estantes = 0
    contador_zorros   = 0
    ref_extrusora     = None
    TAMANO_LOTE_OPERARIO = 5
    TIEMPO_CARGUE_HORAS  = 5 / 3600

    while True:
        with ext.request() as req_maquina:
            yield req_maquina
            t_inicio_uso = env.now
            fin_jornada  = t_inicio_uso + 8

            while env.now < fin_jornada:
                if len(bm.items) < TAMANO_LOTE_OPERARIO:
                    yield env.timeout(0.01)
                    continue

                primer_bloque = yield bm.get()
                ref_lote = primer_bloque["ref"]

                if ref_extrusora is not None and ref_lote != ref_extrusora:
                    t_setup = REFS[ref_lote]["setup_extrusora_h"]
                    if t_setup > 0:
                        estado.util_maquinas["setup"] += t_setup
                        yield env.timeout(t_setup)
                    if estante_actual["ref"] not in (None, ref_lote) and estante_actual["bloques"] > 0:
                        contador_estantes += 1
                        yield ce.put({"ref": estante_actual["ref"],
                                      "bloques": estante_actual["bloques"],
                                      "id": contador_estantes})
                        estante_actual = {"ref": None, "bloques": 0, "id": 0}
                    if zorro_actual["ref"] not in (None, ref_lote) and zorro_actual["bloques"] > 0:
                        yield zp.put(1)
                        zorro_actual = {"ref": None, "bloques": 0}

                ref_extrusora = ref_lote
                yield env.timeout(tiempo_produccion() / 10000)

                lote = [primer_bloque]
                for _ in range(TAMANO_LOTE_OPERARIO - 1):
                    lote.append((yield bm.get()))

                # Defectos de extrusión — con registro por batch (NUEVO)
                lote_ok = []
                for bloque in lote:
                    ref_b = bloque["ref"]
                    estado.defectos_etapa["extrusion"]["inspeccionados"] += 1
                    b_idx = batch_activo(env.now, TIEMPO_CALENTAMIENTO, HORAS_POR_BATCH)
                    if b_idx is not None:
                        estado.batches_kpi[b_idx]["def_ext_insp"] += 1
                    if random.random() > REFS[ref_b]["tasa_defecto_ext"]:
                        lote_ok.append(bloque)
                    else:
                        estado.defectos_etapa["extrusion"]["defectuosos"] += 1
                        estado.kpi_ref[ref_b]["defecto_ext"] += 1
                        if b_idx is not None:
                            estado.batches_kpi[b_idx]["def_ext_def"] += 1

                with op.request() as req_op:
                    yield req_op
                    t_op = env.now
                    yield env.timeout(TIEMPO_CARGUE_HORAS)
                    estado.util_operarios["produccion"] += env.now - t_op

                    for bloque in lote_ok:
                        ref_b     = bloque["ref"]
                        cap_est   = REFS[ref_b]["cap_estante"]
                        cap_zorro = REFS[ref_b]["cap_zorro_humedo"]
                        estado.contador_produccion["total"] += 1
                        estado.kpi_ref[ref_b]["produccion"] += 1

                        if ed.level > 0 or \
                           (estante_actual["bloques"] > 0 and estante_actual["ref"] == ref_b):

                            if estante_actual["ref"] not in (None, ref_b) and estante_actual["bloques"] > 0:
                                contador_estantes += 1
                                yield ce.put({"ref": estante_actual["ref"],
                                              "bloques": estante_actual["bloques"],
                                              "id": contador_estantes})
                                estante_actual = {"ref": None, "bloques": 0, "id": 0}

                            if estante_actual["bloques"] == 0:
                                yield ed.get(1)
                                estante_actual["ref"] = ref_b

                            estante_actual["bloques"] += 1
                            if estante_actual["bloques"] >= cap_est:
                                contador_estantes += 1
                                yield ce.put({"ref": ref_b,
                                              "bloques": estante_actual["bloques"],
                                              "id": contador_estantes})
                                estante_actual = {"ref": None, "bloques": 0, "id": 0}
                        else:
                            if zorro_actual["ref"] not in (None, ref_b) and zorro_actual["bloques"] > 0:
                                yield zp.put(1)
                                zorro_actual = {"ref": None, "bloques": 0}

                            if zorro_actual["bloques"] == 0:
                                yield zp.get(1)
                                zorro_actual["ref"] = ref_b

                            zorro_actual["bloques"] += 1
                            if zorro_actual["bloques"] >= cap_zorro:
                                contador_zorros += 1
                                ref_zorro = zorro_actual["ref"]
                                zorro_actual = {"ref": None, "bloques": 0}
                                if contador_zorros % 4 == 0:
                                    env.process(proceso_secado_natural(env, estado, ref_zorro))

            estado.util_maquinas["extrusion"] += env.now - t_inicio_uso
        yield env.timeout(16)

# ============================================================
# --- CONTROLADOR SECADO ARTIFICIAL ---
# ============================================================
def controlador_secado_artificial(env, estado):
    while True:
        estante = yield estado._ce.get()
        env.process(proceso_secado_artificial(env, estado, estante))
        yield env.timeout(0.5)

# ============================================================
# --- PROCESO SECADO ARTIFICIAL ---
# ============================================================
def proceso_secado_artificial(env, estado, estante):
    ref       = estante["ref"]
    n_bloques = estante["bloques"]
    estado.estantes_en_secado["count"] += 1

    with estado._sa.request() as req:
        yield req
        yield env.timeout(TIEMPO_SECADO)

    estado.estantes_en_secado["count"] -= 1

    with estado._op_endague_secnat.request() as req_op:
        yield req_op
        t_op = env.now
        tasa_def = REFS[ref]["tasa_defecto_secart"]
        estado.defectos_etapa["secado_artificial"]["inspeccionados"] += n_bloques
        defectuosas = int(np.random.binomial(n_bloques, tasa_def))
        buenas      = n_bloques - defectuosas
        estado.defectos_etapa["secado_artificial"]["defectuosos"] += defectuosas
        estado.kpi_ref[ref]["defecto_secart"] += defectuosas
        estado.util_operarios["endague"] += env.now - t_op

        b_idx = batch_activo(env.now, TIEMPO_CALENTAMIENTO, HORAS_POR_BATCH)
        if b_idx is not None:
            bk = estado.batches_kpi[b_idx]
            bk["def_secart_insp"] += n_bloques
            bk["def_secart_def"]  += defectuosas

        if defectuosas > 0:
            yield estado._ba.put(defectuosas / REFS[ref]["bloques_por_ton"])
        if buenas > 0:
            yield env.timeout(3 / 60)
            yield estado._apq[ref].put(buenas)
            estado.contador_secart["total"] += buenas
            estado.kpi_ref[ref]["secart"]   += buenas

    yield estado._ed.put(1)

# ============================================================
# --- PROCESO SECADO NATURAL ---
# ============================================================
def proceso_secado_natural(env, estado, ref):
    unidades = REFS[ref]["cap_zorro_humedo"] * 4
    yield env.timeout(5 / 60)

    if (estado._pv.level + unidades) <= CAPACIDAD_PATIO_VENTILADOR:
        yield estado._pv.put(unidades)
        env.process(secado_lote(env, estado, unidades, TIEMPO_SECADO_VENTILADOR,
                                estado._pv, ref))
    else:
        yield estado._pn.put(unidades)
        env.process(secado_lote(env, estado, unidades, TIEMPO_SECADO_NATURAL,
                                estado._pn, ref))

    yield env.timeout(5 / 60)
    yield estado._zp.put(4)
    estado.contador_secnat["zorros"] += 1

# ============================================================
# --- SECADO LOTE ---
# ============================================================
def secado_lote(env, estado, unidades_lote, tiempo_secado, patio, ref):
    yield env.timeout(tiempo_secado)
    yield patio.get(unidades_lote)

    with estado._mvh.request() as turno:
        yield turno
        yield estado._zh.get(4)

        with estado._op_endague_secnat.request() as req:
            yield req
            t_op = env.now
            tasa_def    = REFS[ref]["tasa_defecto_secnat"]
            estado.defectos_etapa["secado_natural"]["inspeccionados"] += unidades_lote
            defectuosas = int(np.random.binomial(unidades_lote, tasa_def))
            buenas      = unidades_lote - defectuosas
            estado.defectos_etapa["secado_natural"]["defectuosos"] += defectuosas
            estado.kpi_ref[ref]["defecto_secnat"] += defectuosas
            estado.contador_secnat["proceso"]     += unidades_lote
            estado.contador_secnat["defectuosas"] += defectuosas
            estado.contador_secnat["buenas"]      += buenas

            b_idx = batch_activo(env.now, TIEMPO_CALENTAMIENTO, HORAS_POR_BATCH)
            if b_idx is not None:
                bk = estado.batches_kpi[b_idx]
                bk["def_secnat_insp"] += unidades_lote
                bk["def_secnat_def"]  += defectuosas

            if defectuosas > 0:
                yield estado._ba.put(defectuosas / REFS[ref]["bloques_por_ton"])
            estado.util_operarios["endague"] += env.now - t_op
            yield env.timeout(10 / 60)

        if buenas > 0:
            yield estado._apq[ref].put(buenas)
            estado.contador_secnat["total"] += buenas
            estado.kpi_ref[ref]["secnat"]   += buenas
            yield env.timeout(5 / 60)

        yield estado._zh.put(4)

# ============================================================
# --- PROCESO CÁMARA ---
# ============================================================
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
        estado_actual = ORDEN_ESTADOS[idx % 4]

        if estado_actual == "endague":
            ref_camara, alm = yield env.process(elegir_almacen_con_lote(env, estado))
            cap_lote_cam    = REFS[ref_camara]["cap_lote"]
            yield alm.get(cap_lote_cam)
            t_inicio = env.now
            with estado._op_endague.request() as req:
                yield req
                t_op = env.now
                yield env.timeout(tiempo_endague())
                estado.util_operarios["endague"] += env.now - t_op
            transcurrido = env.now - t_inicio
            if transcurrido < 24:
                yield env.timeout(24 - transcurrido)

        elif estado_actual in ("quema", "coccion"):
            yield env.timeout(24)

        elif estado_actual == "deshorne":
            t_inicio = env.now
            with estado._op_deshorne.request() as req:
                yield req
                t_op = env.now
                yield env.timeout(tiempo_deshorne())
                estado.util_operarios["deshorne"] += env.now - t_op

            buenas = reproceso = chamote = 0
            tasa_def = REFS[ref_camara]["tasa_defecto_horno"]
            estado.defectos_etapa["horno"]["inspeccionados"] += cap_lote_cam

            b_idx = batch_activo(env.now, TIEMPO_CALENTAMIENTO, HORAS_POR_BATCH)
            if b_idx is not None:
                estado.batches_kpi[b_idx]["def_horno_insp"] += cap_lote_cam

            for _ in range(cap_lote_cam):
                if random.random() < tasa_def:
                    estado.defectos_etapa["horno"]["defectuosos"]  += 1
                    estado.kpi_ref[ref_camara]["defecto_horno"]     += 1
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

            estado.contador_salida["total"]          += buenas
            estado.contador_salida["chamote"]         += chamote
            estado.contador_salida["reproceso_horno"] += reproceso
            estado.contador_salida["deshornadas_acum"] += buenas + chamote + reproceso
            estado.kpi_ref[ref_camara]["producto_final"] += buenas
            estado.kpi_ref[ref_camara]["chamote"]        += chamote
            estado.kpi_ref[ref_camara]["reproceso"]      += reproceso

            if b_idx is not None:
                bk = estado.batches_kpi[b_idx]
                bk["throughput_bloques"]           += buenas
                bk[f"throughput_{ref_camara}"]     += buenas        # NUEVO v2
                bk[f"producto_final_{ref_camara}"] += buenas        # NUEVO v2
                bk[f"chamote_{ref_camara}"]        += chamote       # NUEVO v2

            n_salida = buenas + chamote + reproceso
            ts_ref   = estado.timestamps_entrada[ref_camara]
            for _ in range(min(n_salida, len(ts_ref))):
                dur = env.now - ts_ref.pop(0)
                estado.ciclos_completados.append(dur)
                estado.kpi_ref[ref_camara]["ciclos_h"].append(dur)

            transcurrido = env.now - t_inicio
            if transcurrido < 24:
                yield env.timeout(24 - transcurrido)

        idx += 1

def proceso_horno(env, estado):
    for i in range(NUM_CAMARAS):
        estado_inicial = ORDEN_ESTADOS[i % 4]
        offset = (i // 4) * (24 / CAMARAS_POR_ESTADO)
        env.process(proceso_camara(env, estado, estado_inicial, offset))
    yield env.timeout(0)

# ============================================================
# --- FUNCIÓN DE EXPORTACIÓN A CSV (NUEVO) ---
# ============================================================
def exportar_resultados(escenario, replica, estado, rows_batches,
                         rows_globales, rows_autocorr):
    """Convierte los acumuladores de estado en filas para los CSV.
    E2 incluye KPIs adicionales de campaña (horas por referencia por batch)."""

    dias_sim = TIEMPO_SIMULACION / 24
    tiempos  = np.array(estado.serie_monitor["tiempo"])

    for b_idx in range(NUM_BATCHES):
        bk = estado.batches_kpi[b_idx]

        t_ini = TIEMPO_CALENTAMIENTO + b_idx * HORAS_POR_BATCH
        t_fin = TIEMPO_CALENTAMIENTO + (b_idx + 1) * HORAS_POR_BATCH

        mask_b = (tiempos >= t_ini) & (tiempos < t_fin)

        def media_batch(clave):
            arr = np.array(estado.serie_monitor[clave])
            return float(arr[mask_b].mean()) if mask_b.any() else float("nan")

        tp_bloques = bk["throughput_bloques"]
        tp_dia     = tp_bloques / DIAS_POR_BATCH
        tp_hora    = tp_bloques / HORAS_POR_BATCH

        def tasa(d, i): return d / i if i > 0 else float("nan")
        tasa_ext    = tasa(bk["def_ext_def"],    bk["def_ext_insp"])
        tasa_secart = tasa(bk["def_secart_def"], bk["def_secart_insp"])
        tasa_secnat = tasa(bk["def_secnat_def"], bk["def_secnat_insp"])
        tasa_horno  = tasa(bk["def_horno_def"],  bk["def_horno_insp"])

        util_molino    = bk["util_molino_h"]    / (HORAS_POR_BATCH * 1)
        util_extrusion = bk["util_extrusion_h"] / (HORAS_POR_BATCH * 1)
        util_op_prod   = bk["util_op_produccion_h"] / (HORAS_POR_BATCH * 5)
        util_op_end    = bk["util_op_endague_h"]    / (HORAS_POR_BATCH * 6)
        util_op_des    = bk["util_op_deshorne_h"]   / (HORAS_POR_BATCH * 4)

        # Fracción de campaña dedicada a cada referencia dentro del batch
        h_total_camp = sum(bk[f"horas_campana_{r}"] for r in REFS)
        frac_N4 = bk["horas_campana_N4"] / h_total_camp if h_total_camp > 0 else float("nan")
        frac_N3 = bk["horas_campana_N3"] / h_total_camp if h_total_camp > 0 else float("nan")
        frac_N5 = bk["horas_campana_N5"] / h_total_camp if h_total_camp > 0 else float("nan")

        kpis = {
            "throughput_bloques":              tp_bloques,
            "throughput_bloques_por_dia":      tp_dia,
            "throughput_bloques_por_hora":     tp_hora,
            # Throughput y producto final por referencia (NUEVO v2)
            "throughput_N4_por_hora":          bk["throughput_N4"] / HORAS_POR_BATCH,
            "throughput_N3_por_hora":          bk["throughput_N3"] / HORAS_POR_BATCH,
            "throughput_N5_por_hora":          bk["throughput_N5"] / HORAS_POR_BATCH,
            "producto_final_N4_por_hora":      bk["producto_final_N4"] / HORAS_POR_BATCH,
            "producto_final_N3_por_hora":      bk["producto_final_N3"] / HORAS_POR_BATCH,
            "producto_final_N5_por_hora":      bk["producto_final_N5"] / HORAS_POR_BATCH,
            "tasa_defecto_extrusion":          tasa_ext,
            "tasa_defecto_secado_artificial":  tasa_secart,
            "tasa_defecto_secado_natural":     tasa_secnat,
            "tasa_defecto_horno":              tasa_horno,
            "nivel_buffer_molienda":           media_batch("buffer_molienda"),
            "nivel_cola_estantes":             media_batch("cola_estantes"),
            "nivel_almacen_preq_N4":           media_batch("apq_N4"),
            "nivel_almacen_preq_N3":           media_batch("apq_N3"),
            "nivel_almacen_preq_N5":           media_batch("apq_N5"),
            "ocupacion_secador_artificial":    media_batch("secado_art_ocup"),
            "ocupacion_patio_ventilador":      media_batch("patio_ventilador"),
            "ocupacion_patio_natural":         media_batch("patio_natural"),
            "utilizacion_molino":              util_molino,
            "utilizacion_extrusora":           util_extrusion,
            "utilizacion_op_produccion":       util_op_prod,
            "utilizacion_op_endague":          util_op_end,
            "utilizacion_op_deshorne":         util_op_des,
            # KPIs exclusivos E2: fracción de campaña por referencia
            "fraccion_campana_N4":             frac_N4,
            "fraccion_campana_N3":             frac_N3,
            "fraccion_campana_N5":             frac_N5,
            "horas_campana_N4_batch":          bk["horas_campana_N4"],
            "horas_campana_N3_batch":          bk["horas_campana_N3"],
            "horas_campana_N5_batch":          bk["horas_campana_N5"],
        }

        for kpi, valor in kpis.items():
            rows_batches.append({
                "escenario": escenario,
                "replica":   replica,
                "batch":     b_idx + 1,
                "t_ini_h":   t_ini,
                "t_fin_h":   t_fin,
                "kpi":       kpi,
                "valor":     valor,
            })

    # Estadísticos globales por réplica
    cs = estado.contador_salida
    arr_ciclos = np.array(estado.ciclos_completados)

    globales = {
        "throughput_total_bloques":      cs["total"],
        "throughput_dia_global":         cs["total"] / dias_sim,
        "throughput_hora_global":        cs["total"] / TIEMPO_SIMULACION,
        # Por referencia (NUEVO v2)
        "producto_final_N4":             estado.kpi_ref["N4"]["producto_final"],
        "producto_final_N3":             estado.kpi_ref["N3"]["producto_final"],
        "producto_final_N5":             estado.kpi_ref["N5"]["producto_final"],
        "throughput_dia_N4":             estado.kpi_ref["N4"]["producto_final"] / dias_sim,
        "throughput_dia_N3":             estado.kpi_ref["N3"]["producto_final"] / dias_sim,
        "throughput_dia_N5":             estado.kpi_ref["N5"]["producto_final"] / dias_sim,
        "tasa_def_extrusion_global":
            (estado.defectos_etapa["extrusion"]["defectuosos"] /
             max(estado.defectos_etapa["extrusion"]["inspeccionados"], 1)),
        "tasa_def_secart_global":
            (estado.defectos_etapa["secado_artificial"]["defectuosos"] /
             max(estado.defectos_etapa["secado_artificial"]["inspeccionados"], 1)),
        "tasa_def_secnat_global":
            (estado.defectos_etapa["secado_natural"]["defectuosos"] /
             max(estado.defectos_etapa["secado_natural"]["inspeccionados"], 1)),
        "tasa_def_horno_global":
            (estado.defectos_etapa["horno"]["defectuosos"] /
             max(estado.defectos_etapa["horno"]["inspeccionados"], 1)),
        "ciclos_n":            len(arr_ciclos),
        "ciclo_media_h":       float(arr_ciclos.mean())       if len(arr_ciclos) else float("nan"),
        "ciclo_mediana_h":     float(np.median(arr_ciclos))   if len(arr_ciclos) else float("nan"),
        "ciclo_min_h":         float(arr_ciclos.min())        if len(arr_ciclos) else float("nan"),
        "ciclo_max_h":         float(arr_ciclos.max())        if len(arr_ciclos) else float("nan"),
        "ciclo_p5_h":          float(np.percentile(arr_ciclos,  5)) if len(arr_ciclos) else float("nan"),
        "ciclo_p95_h":         float(np.percentile(arr_ciclos, 95)) if len(arr_ciclos) else float("nan"),
        # E2: resumen de campañas
        "campanas_N4":         estado.kpi_ref["N4"]["campanas"],
        "campanas_N3":         estado.kpi_ref["N3"]["campanas"],
        "campanas_N5":         estado.kpi_ref["N5"]["campanas"],
        "horas_campana_N4":    estado.kpi_ref["N4"]["horas_campana"],
        "horas_campana_N3":    estado.kpi_ref["N3"]["horas_campana"],
        "horas_campana_N5":    estado.kpi_ref["N5"]["horas_campana"],
    }

    for kpi, valor in globales.items():
        rows_globales.append({
            "escenario": escenario,
            "replica":   replica,
            "kpi":       kpi,
            "valor":     valor,
        })

    # Autocorrelación lag-1
    kpis_para_autocorr = [
        "throughput_bloques",
        "tasa_defecto_extrusion",
        "tasa_defecto_secado_artificial",
        "tasa_defecto_secado_natural",
        "tasa_defecto_horno",
        "nivel_buffer_molienda",
        "nivel_cola_estantes",
        "utilizacion_molino",
        "utilizacion_extrusora",
        "utilizacion_op_produccion",
        "utilizacion_op_endague",
        "utilizacion_op_deshorne",
        "ocupacion_secador_artificial",
        "ocupacion_patio_ventilador",
        "ocupacion_patio_natural",
        "fraccion_campana_N4",
        "fraccion_campana_N3",
        "fraccion_campana_N5",
    ]

    batch_rows_replica = [r for r in rows_batches
                          if r["escenario"] == escenario and r["replica"] == replica]

    for kpi_nombre in kpis_para_autocorr:
        serie = [r["valor"] for r in batch_rows_replica if r["kpi"] == kpi_nombre]
        if len(serie) >= 3:
            arr_s = np.array(serie, dtype=float)
            mask_valid = ~np.isnan(arr_s)
            arr_s = arr_s[mask_valid]
            if len(arr_s) >= 3 and arr_s.std() > 0:
                lag1 = float(np.corrcoef(arr_s[:-1], arr_s[1:])[0, 1])
            else:
                lag1 = float("nan")
        else:
            lag1 = float("nan")

        rows_autocorr.append({
            "escenario":    escenario,
            "replica":      replica,
            "kpi":          kpi_nombre,
            "num_batches":  NUM_BATCHES,
            "autocorr_lag1": lag1,
            "advertencia":  "REVISAR" if abs(lag1) > 0.3 and not np.isnan(lag1) else "OK",
        })

# ============================================================
# --- FUNCIÓN PRINCIPAL: ejecutar una réplica completa ---
# ============================================================
def ejecutar_replica(replica_id, verbose=True):
    semilla = SEMILLA_BASE + replica_id
    random.seed(semilla)
    np.random.seed(semilla)

    if verbose:
        print(f"  [E2] Réplica {replica_id:02d} | semilla={semilla}", end=" ", flush=True)

    env    = simpy.Environment()
    estado = EstadoReplica()

    # Recursos SimPy
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

    # Lanzar todos los procesos desde t=0, incluido el horno (CAMBIO v2)
    # El horno arranca durante el warm-up para alcanzar estado estable.
    env.process(proceso_campanas(env, estado))
    env.process(proceso_molienda(env, estado))
    env.process(proceso_produccion(env, estado))
    env.process(controlador_secado_artificial(env, estado))
    env.process(proceso_horno(env, estado))          # CAMBIO v2: antes era post-warmup
    env.process(proceso_monitor(env, estado, intervalo=1.0))
    env.process(proceso_monitor_diario(env, estado, intervalo=24.0))

    # Fase 1: Warm-up — todos los procesos activos
    env.run(until=TIEMPO_CALENTAMIENTO)

    # CAMBIO v2: resetear contadores al fin del warm-up
    # Los recursos y buffers conservan su estado estable.
    estado.reset_post_warmup()

    # Lanzar batch monitor al inicio de la fase efectiva
    env.process(proceso_batch_monitor(env, estado))

    # Fase 2: Simulación efectiva
    env.run(until=TIEMPO_CALENTAMIENTO + TIEMPO_SIMULACION)

    if verbose:
        print(f"→ horno={estado.contador_salida['total']:,} bloques")

    return estado

# ============================================================
# --- VERIFICACIÓN DE COHERENCIA ---
# ============================================================
def verificar_coherencia(estado, replica_id):
    total_mol    = estado.contador_produccion["molienda"]
    total_buenos = estado.contador_salida["total"]
    chamote      = estado.contador_salida.get("chamote", 0)
    d_ext        = estado.defectos_etapa["extrusion"]["defectuosos"]
    d_sa         = estado.defectos_etapa["secado_artificial"]["defectuosos"]
    d_sn         = estado.defectos_etapa["secado_natural"]["defectuosos"]
    total_cont   = total_buenos + chamote + d_ext + d_sa + d_sn
    dif          = total_mol - total_cont
    ok_balance   = abs(dif) / max(total_mol, 1) < 0.15

    zorros_vis  = estado._zp.level + estado._zh.level
    est_tot     = estado._ed.level + estado.estantes_en_secado["count"] + len(estado._ce.items)
    ok_zorros   = zorros_vis <= 13
    ok_estantes = est_tot == CANTIDAD_ESTANTES_TOTALES

    print(f"  [Coherencia R{replica_id:02d}] "
          f"Mol={total_mol:,} Cont={total_cont:,} WIP={dif:,} "
          f"Balance={'✓' if ok_balance else '⚠'} "
          f"Zorros={zorros_vis}{'✓' if ok_zorros else '⚠'} "
          f"Estantes={est_tot}{'✓' if ok_estantes else '⚠'}")

# ============================================================
# --- BLOQUE PRINCIPAL ---
# ============================================================
if __name__ == "__main__":
    os.makedirs(DIR_SALIDA, exist_ok=True)

    ciclo_h = sum(DURACION_CAMPANA.values())
    print("=" * 65)
    print("  ESCENARIO 2 — PRODUCCIÓN POR CAMPAÑAS")
    print(f"  Ciclo: " + " → ".join(f"{r}({DURACION_CAMPANA[r]}h)" for r in ORDEN_CAMPANAS)
          + f"  = {ciclo_h}h/ciclo")
    print(f"  {NUM_REPLICAS} réplicas × {NUM_BATCHES} batches × {DIAS_POR_BATCH:.0f} días/batch")
    print(f"  ({NUM_BATCHES} batches × {ciclo_h}h × 6 ciclos = {NUM_BATCHES*ciclo_h*6/24:.0f} días cubiertos)")
    print(f"  Warm-up: {TIEMPO_CALENTAMIENTO/24:.0f} días | Simulación: {TIEMPO_SIMULACION/24:.0f} días")
    print(f"  Semilla base: {SEMILLA_BASE}")
    print("=" * 65)

    rows_batches  = []
    rows_globales = []
    rows_autocorr = []

    for r in range(1, NUM_REPLICAS + 1):
        estado = ejecutar_replica(r, verbose=True)
        verificar_coherencia(estado, r)
        exportar_resultados(
            escenario="E2_campanas",
            replica=r,
            estado=estado,
            rows_batches=rows_batches,
            rows_globales=rows_globales,
            rows_autocorr=rows_autocorr,
        )

    ruta_b = os.path.join(DIR_SALIDA, "E2_batches.csv")
    ruta_g = os.path.join(DIR_SALIDA, "E2_globales.csv")
    ruta_a = os.path.join(DIR_SALIDA, "E2_autocorrelacion.csv")

    pd.DataFrame(rows_batches).to_csv(ruta_b,  index=False)
    pd.DataFrame(rows_globales).to_csv(ruta_g, index=False)
    pd.DataFrame(rows_autocorr).to_csv(ruta_a, index=False)


    print(f"\n  CSV exportados en '{DIR_SALIDA}/':")
    print(f"    {ruta_b}")
    print(f"    {ruta_g}")
    print(f"    {ruta_a}")

    df_ac = pd.DataFrame(rows_autocorr)
    advertencias = df_ac[df_ac["advertencia"] == "REVISAR"]
    if not advertencias.empty:
        print("\n  ⚠  KPIs con autocorrelación lag-1 > 0.3 (considerar más batches):")
        for kpi in advertencias["kpi"].unique():
            vals = advertencias[advertencias["kpi"] == kpi]["autocorr_lag1"]
            print(f"     {kpi}: media={vals.mean():.3f}")
    else:
        print("\n  ✓  Todas las autocorrelaciones lag-1 dentro del umbral (|r|≤0.3)")

    print("\n" + "=" * 65)
