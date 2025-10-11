import polars as pl

BKS = {
    "tsp": pl.read_csv("bks/tsp_bks.csv").with_columns(pl.concat_str(pl.col("instance"), pl.lit(".tsp")).alias("instance")),
    "csp": pl.read_csv("bks/csp_bks.csv"),
    "pdptw": pl.read_csv("bks/pdptw_bks.csv"),
}
