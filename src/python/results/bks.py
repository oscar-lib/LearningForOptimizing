import polars as pl
import os


if os.getcwd().endswith(os.path.join("src", "python")):
    os.chdir("../../")

BKS = {
    "tsp": pl.read_csv("bks/tsp_bks.csv").with_columns(instance=pl.concat_str(pl.col("instance"), pl.lit(".tsp"))),
    "csp": pl.read_csv("bks/csp_bks.csv"),
    "pdptw": pl.read_csv("bks/pdptw_bks.csv"),
}


def primal_gap(obj: float, bks: float, is_feasible: bool):
    if not is_feasible:
        return 1.0
    if obj == bks:
        return 0.0
    return abs(bks - obj) / max(abs(bks), abs(obj))
