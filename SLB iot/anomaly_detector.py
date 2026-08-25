"""
anomaly_detector.py

Reads sensor_data.csv and flags anomalies using a rolling z-score method.

Why z-score (and not something fancier) for the first version:
- It's easy to explain in an interview: "a reading is flagged if it's more
  than N standard deviations from the recent rolling average"
- It works well on single-sensor time series without needing training data
- It's a natural stepping stone to explain *why* you'd later upgrade to
  Isolation Forest (multivariate anomalies - e.g. pressure+vibration both
  slightly off at the same time, which z-score per-column would miss)

This file can be run standalone (prints flagged rows) or imported by
dashboard.py to reuse the same detection logic.
"""

import pandas as pd

CSV_FILE = "sensor_data.csv"
ROLLING_WINDOW = 20   # number of past readings to base "normal" on
Z_THRESHOLD = 3.0      # flag if more than 3 std devs from rolling mean

SENSOR_COLUMNS = ["pressure", "temperature", "vibration"]


def detect_anomalies(df: pd.DataFrame) -> pd.DataFrame:
    """
    Given a DataFrame with sensor columns, add:
      - {col}_zscore for each sensor
      - is_anomaly_detected (True if ANY sensor exceeds Z_THRESHOLD)

    Returns the DataFrame with these new columns added.
    """
    df = df.copy()

    for col in SENSOR_COLUMNS:
        rolling_mean = df[col].rolling(window=ROLLING_WINDOW, min_periods=5).mean()
        rolling_std = df[col].rolling(window=ROLLING_WINDOW, min_periods=5).std()
        # avoid divide-by-zero when std is 0 or NaN
        df[f"{col}_zscore"] = (df[col] - rolling_mean) / rolling_std.replace(0, pd.NA)

    zscore_cols = [f"{col}_zscore" for col in SENSOR_COLUMNS]
    df["is_anomaly_detected"] = (df[zscore_cols].abs() > Z_THRESHOLD).any(axis=1)

    return df


def main():
    df = pd.read_csv(CSV_FILE)
    if df.empty:
        print("No data yet - run sensor_simulator.py first.")
        return

    result = detect_anomalies(df)
    flagged = result[result["is_anomaly_detected"]]

    print(f"Total readings: {len(result)}")
    print(f"Flagged as anomalies: {len(flagged)}")
    if not flagged.empty:
        print("\nFlagged rows:")
        print(flagged[["timestamp"] + SENSOR_COLUMNS + ["is_anomaly_injected"]])


if __name__ == "__main__":
    main()