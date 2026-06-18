import simpy
import numpy as np
import random
import pandas as pd
import os

# ============================================================
# ESCENARIO BASE — PRODUCCIÓN REFERENCIA N4 (única referencia)  (v2)
# Modelo original sin modificaciones de lógica operativa.
# Se añade instrumentación Batch Means para análisis estadístico.
# ============================================================

# --- PARÁMETROS DEL SISTEMA ---
CANTIDAD_ESTANTES_TOTALES = 116
CAPACIDAD_ESTANTE         = 280
CAPACIDAD_SECADOR         = 60
TIEMPO_SECADO             = 30
BLOQUES_POR_TON           = 1000 / 4.5
CAPACIDAD_ZORRO_HUMEDO    = 48
CAPACIDAD_ZORRO_SECO      = 60
CAPACIDAD_PATIO_VENTILADOR= 26666
CAPACIDAD_PATIO_NATURAL   = 53332
TIEMPO_SECADO_VENTILADOR  = 180
TIEMPO_SECADO_NATURAL     = 240
CAPACIDAD_SECCION_HORNO   = 18200
CAPACIDAD_LOTE            = 3640
CAMARAS_POR_ESTADO        = 5
NUM_CAMARAS               = 20

# ============================================================
# --- PARÁMETROS BATCH MEANS (NUEVO) ---
# El escenario base no tiene ciclos de campaña que alinear,
# por lo que se usa el mismo diseño que E1:
#   13 batches × 24 días = 312 días exactos.
# Esto hace comparables los batches de EB con los de E1 y E2
# cuando se usan tasas/hora como unidad de comparación.
# ============================================================
NUM_REPLICAS    = 10
NUM_BATCHES     = 13
HORAS_POR_BATCH = (24 * 312) / NUM_BATCHES   # 576.0 h = 24 días por batch
DIAS_POR_BATCH  = HORAS_POR_BATCH / 24        # 24.0 días

TIEMPO_CALENTAMIENTO = 24 * 20    # 20 días (validado con Welch)
TIEMPO_SIMULACION    = 24 * 312   # 312 días de simulación efectiva

# Semillas reproducibles. Semilla réplica r = SEMILLA_BASE + r.
# Misma base que E1/E2 para comparaciones con varianza reducida.
SEMILLA_BASE = 42

DIR_SALIDA = "resultados_batch_means"

# ============================================================
# --- FUNCIONES DE DISTRIBUCIÓN (sin cambios) ---
# ============================================================
def tiempo_molienda_bimodal():
    p = random.random()
    if p < 0.2842329:
        valor = np.random.normal(3.464238, 0.8876048)
    else:
        valor = np.random.normal(6.296459, 2.3376494)
    return max(valor, 2)

def tiempo_produccion():
    return np.random.gamma(5.844786, 1 / 1.069074)

def tiempo_endague():
    return np.random.exponential(1090.61688426301) / 3600

def tiempo_deshorne():
    return np.random.exponential(839.79291208413) / 3600

# ============================================================
# --- FUNCIÓN AUXILIAR: batch activo (NUEVO) ---
# Retorna el índice 0-based del batch correspondiente a t_sim,
# o None si el instante cae dentro del warm-up.
# ============================================================
def batch_activo(t_sim):
    t_efectivo = t_sim - TIEMPO_CALENTAMIENTO
    if t_efectivo < 0:
        return None
    b = int(t_efectivo // HORAS_POR_BATCH)
    return min(b, NUM_BATCHES - 1)

# ============================================================
# --- CLASE ESTADO DE RÉPLICA (NUEVO) ---
# Encapsula todos los contadores mutables de una réplica para
# permitir 10 réplicas independientes sin contaminación de estado.
# ============================================================
class EstadoReplica:
    def __init__(self):
        # Contadores originales del modelo base
        self.contador_salida     = {"total": 0, "deshornadas_acum": 0, "chamote": 0, "reproceso_horno": 0}
        self.contador_produccion = {"total": 0, "molienda": 0}
        self.contador_secnat     = {"total": 0, "zorros": 0, "proceso": 0,
                                    "defectuosas": 0, "buenas": 0}
        self.contador_secart     = {"total": 0}

        self.defectos_etapa = {
            "extrusion":         {"inspeccionados": 0, "defectuosos": 0},
            "secado_artificial": {"inspeccionados": 0, "defectuosos": 0},
            "secado_natural":    {"inspeccionados": 0, "defectuosos": 0},
            "horno":             {"inspeccionados": 0, "defectuosos": 0},
        }

        self.ciclos_completados = []
        self.timestamps_entrada = []          # FIFO de tiempos de entrada
        self.ton_molidas_acum   = {"total": 0.0}
        self.util_maquinas      = {"molino": 0.0, "extrusion": 0.0}
        self.util_operarios     = {"produccion": 0.0, "endague": 0.0, "deshorne": 0.0}
        self.estantes_en_secado = {"count": 0}

        self._snap = {"molienda_ton": 0.0, "produccion_und": 0,
                      "deshornadas_und": 0, "producto_final": 0}

        # Serie temporal del monitor (muestreo cada 1 hora)
        self.serie_monitor = {
            "tiempo":            [],
            "buffer_molienda":   [],
            "cola_estantes":     [],
            "almacen_pre_quema": [],
            "secado_art_ocup":   [],
            "patio_ventilador":  [],
            "patio_natural":     [],
            "zorros_prod":       [],
            "zorros_horno":      [],
            "estantes_disp":     [],
            "estantes_secado":   [],
            "cola_est_count":    [],
        }

        self.registro_diario = []

        # --------------------------------------------------------
        # Acumuladores por batch (NUEVO — núcleo de Batch Means)
        # Cada entrada de la lista corresponde a un batch.
        # --------------------------------------------------------
        self.batches_kpi = [
            {
                # Throughput total y por referencia (EB solo tiene N4)
                "throughput_bloques":       0,
                "throughput_N4":            0,   # alias de throughput_bloques para EB
                "producto_final_N4":        0,
                "chamote_N4":               0,
                # Defectos por etapa (conteos para calcular tasas al cierre)
                "def_ext_insp":             0, "def_ext_def":        0,
                "def_secart_insp":          0, "def_secart_def":     0,
                "def_secnat_insp":          0, "def_secnat_def":     0,
                "def_horno_insp":           0, "def_horno_def":      0,
                # Utilización: tiempo de uso acumulado dentro del batch
                "util_molino_h":            0.0,
                "util_extrusion_h":         0.0,
                "util_op_produccion_h":     0.0,
                "util_op_endague_h":        0.0,
                "util_op_deshorne_h":       0.0,
                "horas_batch":              HORAS_POR_BATCH,
            }
            for _ in range(NUM_BATCHES)
        ]

        # Snapshot de utilización al inicio de cada batch para calcular deltas
        self._snap_util_batch = {
            "molino":        0.0,
            "extrusion":     0.0,
            "op_produccion": 0.0,
            "op_endague":    0.0,
            "op_deshorne":   0.0,
        }


# ── Métodos de reset (se añaden fuera de la clase por diseño del original) ───
def _reset_estado(estado):
    """Resetea todos los acumuladores estadísticos de un EstadoReplica.
    Los recursos SimPy NO se modifican: conservan el estado estable."""
    estado.contador_salida     = {"total": 0, "deshornadas_acum": 0,
                                  "chamote": 0, "reproceso_horno": 0}
    estado.contador_produccion = {"total": 0, "molienda": 0}
    estado.contador_secnat     = {"total": 0, "zorros": 0, "proceso": 0,
                                  "defectuosas": 0, "buenas": 0}
    estado.contador_secart     = {"total": 0}
    estado.defectos_etapa = {
        "extrusion":         {"inspeccionados": 0, "defectuosos": 0},
        "secado_artificial": {"inspeccionados": 0, "defectuosos": 0},
        "secado_natural":    {"inspeccionados": 0, "defectuosos": 0},
        "horno":             {"inspeccionados": 0, "defectuosos": 0},
    }
    estado.ciclos_completados = []
    estado.timestamps_entrada = []
    estado.ton_molidas_acum   = {"total": 0.0}
    estado.util_maquinas      = {"molino": 0.0, "extrusion": 0.0}
    estado.util_operarios     = {"produccion": 0.0, "endague": 0.0, "deshorne": 0.0}
    estado._snap = {"molienda_ton": 0.0, "produccion_und": 0,
                    "deshornadas_und": 0, "producto_final": 0}
    estado._snap_util_batch = {
        "molino": 0.0, "extrusion": 0.0,
        "op_produccion": 0.0, "op_endague": 0.0, "op_deshorne": 0.0,
    }


# ============================================================
# --- PROCESO MONITOR DE SERIES TEMPORALES (sin cambios lógicos)
# ============================================================
def proceso_monitor(env, estado, intervalo=1.0):
    s = estado.serie_monitor
    while True:
        yield env.timeout(intervalo)
        s["tiempo"].append(env.now)
        s["buffer_molienda"].append(len(estado._bm.items))
        s["cola_estantes"].append(len(estado._ce.items))
        s["almacen_pre_quema"].append(estado._apq.level)
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
            "dia":                     dia,
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
# Se ejecuta en paralelo a la simulación. Al cierre de cada batch
# captura el delta de utilización de máquinas y operarios respecto
# al snapshot del batch anterior, evitando doble conteo.
# ============================================================
def proceso_batch_monitor(env, estado):
    for b in range(NUM_BATCHES):
        t_fin_batch = TIEMPO_CALENTAMIENTO + (b + 1) * HORAS_POR_BATCH
        yield env.timeout(max(0, t_fin_batch - env.now))

        snap = estado._snap_util_batch
        um   = estado.util_maquinas
        uo   = estado.util_operarios
        bk   = estado.batches_kpi[b]

        # Delta = uso acumulado ahora − uso acumulado al inicio del batch
        bk["util_molino_h"]        = um["molino"]     - snap["molino"]
        bk["util_extrusion_h"]     = um["extrusion"]  - snap["extrusion"]
        bk["util_op_produccion_h"] = uo["produccion"] - snap["op_produccion"]
        bk["util_op_endague_h"]    = uo["endague"]    - snap["op_endague"]
        bk["util_op_deshorne_h"]   = uo["deshorne"]   - snap["op_deshorne"]

        # Actualizar snapshot para el siguiente batch
        snap["molino"]        = um["molino"]
        snap["extrusion"]     = um["extrusion"]
        snap["op_produccion"] = uo["produccion"]
        snap["op_endague"]    = uo["endague"]
        snap["op_deshorne"]   = uo["deshorne"]


# ============================================================
# --- PROCESO MOLIENDA (sin cambios de lógica) ---
# ============================================================
def proceso_molienda(env, estado):
    CAPACIDAD_MAX = 14
    while True:
        with estado._mol.request() as req:
            yield req
            t_inicio_uso = env.now
            fin_jornada  = t_inicio_uso + 12

            while env.now < fin_jornada:
                tasa = min(tiempo_molienda_bimodal(), CAPACIDAD_MAX)
                if estado._ba.level <= 0:
                    yield env.timeout(0.1)
                    continue

                ton_procesadas = min(tasa, estado._ba.level)
                yield estado._ba.get(ton_procesadas)
                estado.ton_molidas_acum["total"] += ton_procesadas

                bloques = int(ton_procesadas * BLOQUES_POR_TON)
                for _ in range(bloques):
                    estado.contador_produccion["molienda"] += 1
                    estado.timestamps_entrada.append(env.now)
                    yield estado._bm.put("bloque")

                yield env.timeout(1)

            estado.util_maquinas["molino"] += env.now - t_inicio_uso
        yield env.timeout(12)


# ============================================================
# --- PROCESO PRODUCCIÓN (sin cambios de lógica) ---
# Se añade registro por batch en el punto de inspección.
# ============================================================
def proceso_produccion(env, estado):
    bloques_en_estante = 0
    bloques_en_zorro   = 0
    contador_estantes  = 0
    contador_zorros    = 0
    TAMANO_LOTE_OPERARIO = 5
    TIEMPO_CARGUE_HORAS  = 5 / 3600

    while True:
        with estado._ext.request() as req_maquina:
            yield req_maquina
            t_inicio_uso = env.now
            fin_jornada  = t_inicio_uso + 8

            while env.now < fin_jornada:
                if len(estado._bm.items) < TAMANO_LOTE_OPERARIO:
                    yield env.timeout(0.01)
                    continue

                yield env.timeout(tiempo_produccion() / 10000)

                lote_para_cargar = []
                for _ in range(TAMANO_LOTE_OPERARIO):
                    bloque = yield estado._bm.get()
                    # Defectos extrusión (~1%) — registro por batch (NUEVO)
                    estado.defectos_etapa["extrusion"]["inspeccionados"] += 1
                    b_idx = batch_activo(env.now)
                    if b_idx is not None:
                        estado.batches_kpi[b_idx]["def_ext_insp"] += 1
                    if random.random() > 0.01:
                        lote_para_cargar.append(bloque)
                    else:
                        estado.defectos_etapa["extrusion"]["defectuosos"] += 1
                        if b_idx is not None:
                            estado.batches_kpi[b_idx]["def_ext_def"] += 1

                with estado._op_prod.request() as req_op:
                    yield req_op
                    t_op = env.now
                    yield env.timeout(TIEMPO_CARGUE_HORAS)
                    estado.util_operarios["produccion"] += env.now - t_op

                    for _ in lote_para_cargar:
                        estado.contador_produccion["total"] += 1

                        if estado._ed.level > 0 or bloques_en_estante > 0:
                            if bloques_en_estante == 0:
                                yield estado._ed.get(1)
                            bloques_en_estante += 1
                            if bloques_en_estante >= CAPACIDAD_ESTANTE:
                                bloques_en_estante = 0
                                contador_estantes += 1
                                yield estado._ce.put(f"Estante_{contador_estantes}")
                        else:
                            if bloques_en_zorro == 0:
                                yield estado._zp.get(1)
                            bloques_en_zorro += 1
                            if bloques_en_zorro >= CAPACIDAD_ZORRO_HUMEDO:
                                bloques_en_zorro = 0
                                contador_zorros += 1
                                if contador_zorros % 4 == 0:
                                    env.process(proceso_secado_natural(env, estado))

            estado.util_maquinas["extrusion"] += env.now - t_inicio_uso
        yield env.timeout(16)


# ============================================================
# --- CONTROLADOR SECADO ARTIFICIAL (sin cambios de lógica) ---
# ============================================================
def controlador_secado_artificial(env, estado):
    while True:
        estante = yield estado._ce.get()
        env.process(proceso_secado_artificial(env, estado))
        yield env.timeout(0.5)


# ============================================================
# --- PROCESO SECADO ARTIFICIAL (sin cambios de lógica) ---
# Se añade registro por batch.
# ============================================================
def proceso_secado_artificial(env, estado):
    estado.estantes_en_secado["count"] += 1

    with estado._sa.request() as req:
        yield req
        yield env.timeout(TIEMPO_SECADO)

    estado.estantes_en_secado["count"] -= 1

    with estado._op_endague_secnat.request() as req_op:
        yield req_op
        t_op = env.now

        unidades_buenas      = 0
        unidades_defectuosas = 0
        estado.defectos_etapa["secado_artificial"]["inspeccionados"] += CAPACIDAD_ESTANTE

        # Registro por batch (NUEVO)
        b_idx = batch_activo(env.now)
        if b_idx is not None:
            estado.batches_kpi[b_idx]["def_secart_insp"] += CAPACIDAD_ESTANTE

        for _ in range(CAPACIDAD_ESTANTE):
            if random.random() < 0.02:
                unidades_defectuosas += 1
                estado.defectos_etapa["secado_artificial"]["defectuosos"] += 1
                if b_idx is not None:
                    estado.batches_kpi[b_idx]["def_secart_def"] += 1
            else:
                unidades_buenas += 1

        estado.util_operarios["endague"] += env.now - t_op

        if unidades_defectuosas > 0:
            yield estado._ba.put(unidades_defectuosas / BLOQUES_POR_TON)
        if unidades_buenas > 0:
            yield env.timeout(3 / 60)
            yield estado._apq.put(unidades_buenas)
            estado.contador_secart["total"] += unidades_buenas

    yield estado._ed.put(1)


# ============================================================
# --- PROCESO SECADO NATURAL (sin cambios de lógica) ---
# ============================================================
def proceso_secado_natural(env, estado):
    unidades = CAPACIDAD_ZORRO_HUMEDO * 4
    yield env.timeout(5 / 60)

    if (estado._pv.level + unidades) <= CAPACIDAD_PATIO_VENTILADOR:
        yield estado._pv.put(unidades)
        env.process(secado_lote(env, estado, unidades, TIEMPO_SECADO_VENTILADOR,
                                estado._pv))
    else:
        yield estado._pn.put(unidades)
        env.process(secado_lote(env, estado, unidades, TIEMPO_SECADO_NATURAL,
                                estado._pn))

    yield env.timeout(5 / 60)
    yield estado._zp.put(4)
    estado.contador_secnat["zorros"] += 1


# ============================================================
# --- SECADO LOTE (sin cambios de lógica) ---
# Se añade registro por batch.
# ============================================================
def secado_lote(env, estado, unidades_lote, tiempo_secado, patio):
    yield env.timeout(tiempo_secado)
    yield patio.get(unidades_lote)

    with estado._mvh.request() as turno:
        yield turno
        yield estado._zh.get(4)

        with estado._op_endague_secnat.request() as req:
            yield req
            t_op = env.now

            estado.defectos_etapa["secado_natural"]["inspeccionados"] += unidades_lote
            defectuosas = int(np.random.binomial(unidades_lote, 0.05))
            buenas      = unidades_lote - defectuosas
            estado.defectos_etapa["secado_natural"]["defectuosos"] += defectuosas

            estado.contador_secnat["proceso"]     += unidades_lote
            estado.contador_secnat["defectuosas"] += defectuosas
            estado.contador_secnat["buenas"]      += buenas

            # Registro por batch (NUEVO)
            b_idx = batch_activo(env.now)
            if b_idx is not None:
                bk = estado.batches_kpi[b_idx]
                bk["def_secnat_insp"] += unidades_lote
                bk["def_secnat_def"]  += defectuosas

            if defectuosas > 0:
                yield estado._ba.put(defectuosas / BLOQUES_POR_TON)

            estado.util_operarios["endague"] += env.now - t_op
            yield env.timeout(10 / 60)

        if buenas > 0:
            yield estado._apq.put(buenas)
            estado.contador_secnat["total"] += buenas
            yield env.timeout(5 / 60)

        yield estado._zh.put(4)


# ============================================================
# --- PROCESO CÁMARA (sin cambios de lógica) ---
# Se añade registro por batch en el deshorne.
# ============================================================
ORDEN_ESTADOS = ["endague", "quema", "coccion", "deshorne"]


def proceso_camara(env, estado, estado_inicial, offset_inicial):
    yield env.timeout(offset_inicial)
    idx = ORDEN_ESTADOS.index(estado_inicial)

    while True:
        estado_actual = ORDEN_ESTADOS[idx % 4]

        if estado_actual == "endague":
            yield estado._apq.get(CAPACIDAD_LOTE)
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
            estado.defectos_etapa["horno"]["inspeccionados"] += CAPACIDAD_LOTE

            # Registro por batch (NUEVO)
            b_idx = batch_activo(env.now)
            if b_idx is not None:
                estado.batches_kpi[b_idx]["def_horno_insp"] += CAPACIDAD_LOTE

            for _ in range(CAPACIDAD_LOTE):
                r = random.random()
                if r < 0.05:
                    estado.defectos_etapa["horno"]["defectuosos"] += 1
                    if b_idx is not None:
                        estado.batches_kpi[b_idx]["def_horno_def"] += 1
                    if random.random() < 0.5:
                        chamote   += 1
                    else:
                        reproceso += 1
                else:
                    buenas += 1

            if reproceso > 0:
                yield estado._ba.put(reproceso / BLOQUES_POR_TON)

            estado.contador_salida["total"]           += buenas
            estado.contador_salida["chamote"]          += chamote
            estado.contador_salida["reproceso_horno"]  += reproceso
            estado.contador_salida["deshornadas_acum"] += buenas + chamote + reproceso

            # Throughput por batch (NUEVO)
            if b_idx is not None:
                bk = estado.batches_kpi[b_idx]
                bk["throughput_bloques"]   += buenas
                bk["throughput_N4"]        += buenas   # NUEVO v2: EB solo produce N4
                bk["producto_final_N4"]    += buenas   # NUEVO v2
                bk["chamote_N4"]           += chamote  # NUEVO v2

            # Tiempo de ciclo — estadístico global por réplica (no por batch)
            n_salida = buenas + chamote + reproceso
            for _ in range(min(n_salida, len(estado.timestamps_entrada))):
                t_entrada = estado.timestamps_entrada.pop(0)
                estado.ciclos_completados.append(env.now - t_entrada)

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
# Genera tres archivos en formato largo (long format):
#   1. EB_batches.csv       — KPIs por (escenario, replica, batch)
#   2. EB_globales.csv      — estadísticos globales por réplica
#   3. EB_autocorrelacion.csv — lag-1 por KPI y réplica
# ============================================================
def exportar_resultados(escenario, replica, estado, rows_batches,
                        rows_globales, rows_autocorr):

    dias_sim = TIEMPO_SIMULACION / 24
    tiempos  = np.array(estado.serie_monitor["tiempo"])

    # -----------------------------------------------------------
    # 1. FILAS POR BATCH
    # -----------------------------------------------------------
    for b_idx in range(NUM_BATCHES):
        bk = estado.batches_kpi[b_idx]

        t_ini = TIEMPO_CALENTAMIENTO + b_idx * HORAS_POR_BATCH
        t_fin = TIEMPO_CALENTAMIENTO + (b_idx + 1) * HORAS_POR_BATCH
        mask_b = (tiempos >= t_ini) & (tiempos < t_fin)

        def media_batch(clave):
            arr = np.array(estado.serie_monitor[clave])
            return float(arr[mask_b].mean()) if mask_b.any() else float("nan")

        # Throughput
        tp_bloques = bk["throughput_bloques"]
        tp_dia     = tp_bloques / DIAS_POR_BATCH
        tp_hora    = tp_bloques / HORAS_POR_BATCH

        # Tasas de defecto por etapa
        def tasa(d, i): return d / i if i > 0 else float("nan")
        tasa_ext    = tasa(bk["def_ext_def"],    bk["def_ext_insp"])
        tasa_secart = tasa(bk["def_secart_def"], bk["def_secart_insp"])
        tasa_secnat = tasa(bk["def_secnat_def"], bk["def_secnat_insp"])
        tasa_horno  = tasa(bk["def_horno_def"],  bk["def_horno_insp"])

        # Utilización de recursos (fracción del tiempo disponible)
        # Molino: 1 máquina; Extrusora: 1 máquina
        # Operarios: producción=5, endague=6 (3 secart + 3 secnat), deshorne=4
        util_molino    = bk["util_molino_h"]        / (HORAS_POR_BATCH * 1)
        util_extrusion = bk["util_extrusion_h"]     / (HORAS_POR_BATCH * 1)
        util_op_prod   = bk["util_op_produccion_h"] / (HORAS_POR_BATCH * 5)
        util_op_end    = bk["util_op_endague_h"]    / (HORAS_POR_BATCH * 6)
        util_op_des    = bk["util_op_deshorne_h"]   / (HORAS_POR_BATCH * 4)

        kpis = {
            "throughput_bloques":              tp_bloques,
            "throughput_bloques_por_dia":      tp_dia,
            "throughput_bloques_por_hora":     tp_hora,
            # Por referencia (NUEVO v2) — EB produce solo N4
            "throughput_N4_por_hora":          bk["throughput_N4"] / HORAS_POR_BATCH,
            "producto_final_N4_por_hora":      bk["producto_final_N4"] / HORAS_POR_BATCH,
            "tasa_defecto_extrusion":          tasa_ext,
            "tasa_defecto_secado_artificial":  tasa_secart,
            "tasa_defecto_secado_natural":     tasa_secnat,
            "tasa_defecto_horno":              tasa_horno,
            "nivel_buffer_molienda":           media_batch("buffer_molienda"),
            "nivel_cola_estantes":             media_batch("cola_estantes"),
            "nivel_almacen_pre_quema":         media_batch("almacen_pre_quema"),
            "ocupacion_secador_artificial":    media_batch("secado_art_ocup"),
            "ocupacion_patio_ventilador":      media_batch("patio_ventilador"),
            "ocupacion_patio_natural":         media_batch("patio_natural"),
            "utilizacion_molino":              util_molino,
            "utilizacion_extrusora":           util_extrusion,
            "utilizacion_op_produccion":       util_op_prod,
            "utilizacion_op_endague":          util_op_end,
            "utilizacion_op_deshorne":         util_op_des,
        }

        for kpi, valor in kpis.items():
            rows_batches.append({
                "escenario": escenario,
                "replica":   replica,
                "batch":     b_idx + 1,      # 1-based para legibilidad
                "t_ini_h":   t_ini,
                "t_fin_h":   t_fin,
                "kpi":       kpi,
                "valor":     valor,
            })

    # -----------------------------------------------------------
    # 2. ESTADÍSTICOS GLOBALES POR RÉPLICA
    # -----------------------------------------------------------
    cs = estado.contador_salida
    arr_ciclos = np.array(estado.ciclos_completados)

    globales = {
        "throughput_total_bloques":  cs["total"],
        "throughput_dia_global":     cs["total"] / dias_sim,
        "throughput_hora_global":    cs["total"] / TIEMPO_SIMULACION,
        # Por referencia (NUEVO v2) — EB produce solo N4
        "producto_final_N4":         cs["total"],
        "throughput_dia_N4":         cs["total"] / dias_sim,
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
        # Tiempo de ciclo: solo estadístico global por réplica (no por batch)
        "ciclos_n":        len(arr_ciclos),
        "ciclo_media_h":   float(arr_ciclos.mean())       if len(arr_ciclos) else float("nan"),
        "ciclo_mediana_h": float(np.median(arr_ciclos))   if len(arr_ciclos) else float("nan"),
        "ciclo_min_h":     float(arr_ciclos.min())        if len(arr_ciclos) else float("nan"),
        "ciclo_max_h":     float(arr_ciclos.max())        if len(arr_ciclos) else float("nan"),
        "ciclo_p5_h":      float(np.percentile(arr_ciclos,  5)) if len(arr_ciclos) else float("nan"),
        "ciclo_p95_h":     float(np.percentile(arr_ciclos, 95)) if len(arr_ciclos) else float("nan"),
    }

    for kpi, valor in globales.items():
        rows_globales.append({
            "escenario": escenario,
            "replica":   replica,
            "kpi":       kpi,
            "valor":     valor,
        })

    # -----------------------------------------------------------
    # 3. AUTOCORRELACIÓN LAG-1 POR KPI Y RÉPLICA (NUEVO)
    # Permite verificar post-hoc que los batches son suficientemente
    # independientes. |r| > 0.3 sugiere aumentar el tamaño de batch.
    # -----------------------------------------------------------
    kpis_para_autocorr = [
        "throughput_bloques",
        "tasa_defecto_extrusion",
        "tasa_defecto_secado_artificial",
        "tasa_defecto_secado_natural",
        "tasa_defecto_horno",
        "nivel_buffer_molienda",
        "nivel_cola_estantes",
        "nivel_almacen_pre_quema",
        "utilizacion_molino",
        "utilizacion_extrusora",
        "utilizacion_op_produccion",
        "utilizacion_op_endague",
        "utilizacion_op_deshorne",
        "ocupacion_secador_artificial",
        "ocupacion_patio_ventilador",
        "ocupacion_patio_natural",
    ]

    batch_rows_replica = [r for r in rows_batches
                          if r["escenario"] == escenario and r["replica"] == replica]

    for kpi_nombre in kpis_para_autocorr:
        serie = [r["valor"] for r in batch_rows_replica if r["kpi"] == kpi_nombre]
        if len(serie) >= 3:
            arr_s = np.array(serie, dtype=float)
            mask_v = ~np.isnan(arr_s)
            arr_s  = arr_s[mask_v]
            if len(arr_s) >= 3 and arr_s.std() > 0:
                lag1 = float(np.corrcoef(arr_s[:-1], arr_s[1:])[0, 1])
            else:
                lag1 = float("nan")
        else:
            lag1 = float("nan")

        rows_autocorr.append({
            "escenario":     escenario,
            "replica":       replica,
            "kpi":           kpi_nombre,
            "num_batches":   NUM_BATCHES,
            "autocorr_lag1": lag1,
            "advertencia":   "REVISAR" if (not np.isnan(lag1) and abs(lag1) > 0.3) else "OK",
        })


# ============================================================
# --- FUNCIÓN PRINCIPAL: ejecutar una réplica completa ---
# ============================================================
def ejecutar_replica(replica_id, verbose=True):
    semilla = SEMILLA_BASE + replica_id
    random.seed(semilla)
    np.random.seed(semilla)

    if verbose:
        print(f"  [EB] Réplica {replica_id:02d} | semilla={semilla}", end=" ", flush=True)

    env    = simpy.Environment()
    estado = EstadoReplica()

    # Recursos SimPy
    estado._mol = simpy.Resource(env, capacity=1)
    estado._ext = simpy.Resource(env, capacity=1)
    estado._sa  = simpy.Resource(env, capacity=CAPACIDAD_SECADOR)
    estado._pv  = simpy.Container(env, capacity=CAPACIDAD_PATIO_VENTILADOR, init=0)
    estado._pn  = simpy.Container(env, capacity=CAPACIDAD_PATIO_NATURAL,    init=0)
    estado._bm  = simpy.Store(env)                               # buffer_molienda
    estado._ce  = simpy.Store(env)                               # cola_estantes
    estado._bmr = simpy.Store(env)                               # buffer reproceso
    estado._zp  = simpy.Container(env, init=6, capacity=6)      # zorros prod
    estado._zh  = simpy.Container(env, init=7, capacity=7)      # zorros horno
    estado._mvh = simpy.Resource(env, capacity=1)                # mutex viaje horno
    estado._ba  = simpy.Container(env, init=100000, capacity=1000000)
    estado._apq = simpy.Container(env, init=0, capacity=10_000_000)  # almacen_pre_quema
    estado._ed  = simpy.Container(env, init=CANTIDAD_ESTANTES_TOTALES,
                                   capacity=CANTIDAD_ESTANTES_TOTALES)
    estado._op_prod           = simpy.Resource(env, capacity=5)
    estado._op_endague        = simpy.Resource(env, capacity=3)
    estado._op_endague_secnat = simpy.Resource(env, capacity=3)
    estado._op_deshorne       = simpy.Resource(env, capacity=4)

    # Lanzar todos los procesos desde t=0, incluido el horno (CAMBIO v2)
    env.process(proceso_molienda(env, estado))
    env.process(proceso_produccion(env, estado))
    env.process(controlador_secado_artificial(env, estado))
    env.process(proceso_horno(env, estado))          # CAMBIO v2: antes era post-warmup
    env.process(proceso_monitor(env, estado, intervalo=1.0))
    env.process(proceso_monitor_diario(env, estado, intervalo=24.0))

    # Fase 1: Warm-up (20 días) — todos los procesos activos
    env.run(until=TIEMPO_CALENTAMIENTO)

    # CAMBIO v2: resetear contadores estadísticos al fin del warm-up
    _reset_estado(estado)

    # Lanzar batch monitor al inicio de la fase efectiva
    env.process(proceso_batch_monitor(env, estado))

    # Fase 2: Simulación efectiva (312 días)
    env.run(until=TIEMPO_CALENTAMIENTO + TIEMPO_SIMULACION)

    if verbose:
        print(f"→ horno={estado.contador_salida['total']:,} bloques")

    return estado


# ============================================================
# --- VERIFICACIÓN DE COHERENCIA (mantenida del modelo base) ---
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
# --- BLOQUE PRINCIPAL: ejecutar todas las réplicas ---
# ============================================================
if __name__ == "__main__":
    os.makedirs(DIR_SALIDA, exist_ok=True)

    print("=" * 65)
    print("  ESCENARIO BASE — PRODUCCIÓN N4 (referencia única)")
    print(f"  {NUM_REPLICAS} réplicas × {NUM_BATCHES} batches × {DIAS_POR_BATCH:.0f} días/batch")
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
            escenario="EB_base",
            replica=r,
            estado=estado,
            rows_batches=rows_batches,
            rows_globales=rows_globales,
            rows_autocorr=rows_autocorr,
        )

    # Guardar CSV
    ruta_b = os.path.join(DIR_SALIDA, "EB_batches.csv")
    ruta_g = os.path.join(DIR_SALIDA, "EB_globales.csv")
    ruta_a = os.path.join(DIR_SALIDA, "EB_autocorrelacion.csv")

    pd.DataFrame(rows_batches).to_csv(ruta_b,  index=False)
    pd.DataFrame(rows_globales).to_csv(ruta_g, index=False)
    pd.DataFrame(rows_autocorr).to_csv(ruta_a, index=False)

    print(f"\n  CSV exportados en '{DIR_SALIDA}/':")
    print(f"    {ruta_b}")
    print(f"    {ruta_g}")
    print(f"    {ruta_a}")

    # Resumen de autocorrelaciones con advertencias
    df_ac        = pd.DataFrame(rows_autocorr)
    advertencias = df_ac[df_ac["advertencia"] == "REVISAR"]
    if not advertencias.empty:
        print("\n  ⚠  KPIs con autocorrelación lag-1 > 0.3 (considerar batches más grandes):")
        for kpi in advertencias["kpi"].unique():
            vals = advertencias[advertencias["kpi"] == kpi]["autocorr_lag1"]
            print(f"     {kpi}: media lag-1 = {vals.mean():.3f}")
    else:
        print("\n  ✓  Todas las autocorrelaciones lag-1 dentro del umbral (|r| ≤ 0.3)")

    print("\n" + "=" * 65)
