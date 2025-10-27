# ======================== LIBRERÍAS ==========================================
from pathlib import Path
import math
import polars as pl
import matplotlib.pyplot as plt

# ======================== CONFIGURACIÓN ======================================
TRANS  = {1, 46, 47, 48, 49, 51}   # clases transicionales
THRESH = 0.1                       # umbral delta

# Transicionales: solo negativos (máx. 20 %)
PERC_TRANS_TOTAL = 0.20            # 20 % del total de la clase

# No transicionales
PERC_NONTRANS_TOTAL = 0.35         # 35 % del total de la clase (neg + pos)
PERC_NONTRANS_POS   = 0.10         # 10 % de los restantes tras quitar negativos

# ======================== CARGA ========================================
BASE_DIR = Path.cwd().parent
ruta = BASE_DIR / "data" / "ALPALPS" / "DatasetNormalizado_probs_ALPALPS.csv"
if not ruta.exists():
    raise FileNotFoundError(f"No se encontró: {ruta}")

df = pl.read_csv(ruta)
print(f"Cargadas {df.shape[0]:,} filas y {df.shape[1]} columnas")

# Detecta columnas de probabilidad
prob_cols = [c for c in df.columns if c.startswith("prob_class_")]

# ======================== PASO 1: AUXILIARES =================================
step1 = (
    df
    .with_columns([
        # probabilidad de la clase propia
        pl.struct(prob_cols + ["labelModelo"])
          .map_elements(
              lambda r: r[f"prob_class_{r['labelModelo']}"],
              return_dtype=pl.Float64,
          ).alias("p_ref"),

        pl.max_horizontal(prob_cols).alias("max_prob"),

        # segunda mejor probabilidad
        pl.struct(prob_cols)
          .map_elements(
              lambda r: sorted(r.values(), reverse=True)[1],
              return_dtype=pl.Float64,
          ).alias("second_max"),
    ])
    .with_columns([
        (pl.col("p_ref") - pl.col("second_max")).alias("delta"),
        (pl.col("p_ref") < pl.col("max_prob")).alias("neg_flag"),
        (pl.col("p_ref") - pl.col("second_max") < THRESH).alias("small_flag"),
        (pl.col("max_prob") - pl.col("p_ref")).alias("gap_neg"),
    ])
)

# ======================== SEPARAR GRUPOS =====================================
step1_trans   = step1.filter(pl.col("labelModelo").is_in(TRANS))
step1_notrans = step1.filter(~pl.col("labelModelo").is_in(TRANS))

# ======================== FUNCIONES DE FILTRADO ==============================
def filtra_transicionales(gdf: pl.DataFrame) -> pl.DataFrame:
    """
    Clase transicional:
      • Se quitan SOLAMENTE negativos (neg_flag=True), empezando por mayor gap_neg,
        hasta eliminar el 20 % de la clase COMO MÁXIMO
        (o todos los negativos si son ≤ 20 %).
    """
    n_total = gdf.height
    cap20   = math.ceil(PERC_TRANS_TOTAL * n_total)

    neg = gdf.filter(pl.col("neg_flag")).sort("gap_neg", descending=True)
    # nº de negativos que realmente vamos a quitar
    drop_neg = neg.head(cap20)

    ids_drop = set(drop_neg["id"].to_list())
    kept = gdf.filter(~pl.col("id").is_in(ids_drop))

    # Limpieza de columnas auxiliares
    return kept.drop([
        "max_prob", "second_max", "delta", "neg_flag",
        "small_flag", "gap_neg"
    ])

def filtra_no_trans(gdf: pl.DataFrame) -> pl.DataFrame:
    """
    Clase NO transicional:
      1) Quitar negativos por gap_neg (desc) hasta el 35 % del total.
      2) Si aún queda margen dentro de ese 35 %, quitar positivos con delta<THRESH
         en orden ascendente de delta, CON EL LÍMITE ADICIONAL de un 10 % de las
         filas que QUEDAN tras eliminar los negativos.
    """
    n_total = gdf.height
    cap35   = math.ceil(PERC_NONTRANS_TOTAL * n_total)

    # --- NEGATIVOS -----------------------------------------------------------
    neg = gdf.filter(pl.col("neg_flag")).sort("gap_neg", descending=True)
    if neg.height <= cap35:
        drop_neg = neg
        cap_left = cap35 - neg.height
    else:
        drop_neg = neg.head(cap35)
        cap_left = 0

    # --- POSITIVOS -----------------------------------------------------------
    if cap_left > 0:
        remaining = n_total - drop_neg.height
        pos_cap10 = math.ceil(PERC_NONTRANS_POS * remaining)

        cand_pos  = (
            gdf
            .filter(~pl.col("neg_flag") & pl.col("small_flag"))
            .sort("delta")                      # delta asc → peores primero
        )
        allowed_pos = min(cap_left, pos_cap10)
        drop_pos = cand_pos.head(allowed_pos)
    else:
        drop_pos = pl.DataFrame(schema=gdf.schema)

    ids_drop = set(drop_neg["id"].to_list()) | set(drop_pos["id"].to_list())
    kept = gdf.filter(~pl.col("id").is_in(ids_drop))

    return kept.drop([
        "max_prob", "second_max", "delta", "neg_flag",
        "small_flag", "gap_neg"
    ])

# ======================== APLICAR FILTROS ====================================
# --- Transicionales ----------------------------------------------------------
trans_final_parts = []
for cls in TRANS:
    sub = step1_trans.filter(pl.col("labelModelo") == cls)
    if sub.height > 0:
        trans_final_parts.append(filtra_transicionales(sub))

trans_final = pl.concat(trans_final_parts) if trans_final_parts else pl.DataFrame()

# --- No transicionales -------------------------------------------------------
notrans_final_parts = []
for cls in step1_notrans.select("labelModelo").unique()["labelModelo"].to_list():
    sub = step1_notrans.filter(pl.col("labelModelo") == cls)
    notrans_final_parts.append(filtra_no_trans(sub))

notrans_final = pl.concat(notrans_final_parts)

# ======================== UNIÓN Y CSV ========================================
df_aux = pl.concat([notrans_final, trans_final]).sort("id")

# CSV del original con TODAS sus columnas, solo filas filtradas
ids_keep = df_aux.select("id")
df_original_filtrado = df.join(ids_keep, on="id", how="semi")
#salida_full = BASE_DIR / "results" / "ALPALPS" / "ALPALPS_original_filtrado_NUEVO.csv"
#df_original_filtrado.write_csv(salida_full)
#print("Guardado original filtrado en:", salida_full)
# ======================== GRÁFICO ANTES/DESPUÉS ==============================
before = (
    df
    .group_by("labelModelo")
    .len()
    .rename({"len": "before"})
    .sort("labelModelo")
)

after = (
    df_original_filtrado
    .group_by("labelModelo")
    .len()
    .rename({"len": "after"})
    .sort("labelModelo")
)

counts = before.join(after, on="labelModelo", how="full").fill_null(0).sort("labelModelo")

# Sin pandas / sin pyarrow
labels = counts["labelModelo"].to_list()
bef    = counts["before"].to_list()
aft    = counts["after"].to_list()

fig, ax = plt.subplots(figsize=(12, 5))
w = 0.4
x = range(len(labels))

ax.bar([i - w/2 for i in x], bef, width=w, label="Antes")
ax.bar([i + w/2 for i in x], aft, width=w, label="Después")

ax.set_xticks(list(x))
ax.set_xticklabels(labels, rotation=90)
ax.set_xlabel("Clase (labelModelo)")
ax.set_ylabel("Número de filas")
ax.set_title("Tamaño de cada clase antes y después del filtrado")
ax.legend()
plt.tight_layout()
plt.show()


print(counts) 
import pandas as pd
#counts_antiguos=counts
pd.set_option("display.max_rows", None)   
tabla = counts.to_pandas()
print(tabla)     


# ======================== GUARDAMOS GRÁFICO Y TABLA ===========================
out_dir = BASE_DIR / "results" / "ALPALPS"     # misma ruta de salida

# 1) Guardar el gráfico
fig.savefig(out_dir / "ALPALPS_morralla_before_after.png", dpi=300)

# 2) Guardar la tabla counts en CSV (sin pasar por pandas)
counts.write_csv(out_dir / "ALPALPS_morralla_before_after.csv")