"""
dashboard.py

Streamlit dashboard for the IIoT Equipment Health Monitoring System.

Run with:  streamlit run dashboard.py

While sensor_simulator.py is running in another terminal (writing to
sensor_data.csv), this dashboard auto-refreshes and shows:
- Live line charts for pressure, temperature, vibration
- A table of the most recent anomalies detected
- Simple KPI cards (current values, total anomalies flagged)
"""

import time

import pandas as pd
import streamlit as st

from anomaly_detector import detect_anomalies, SENSOR_COLUMNS, CSV_FILE

st.set_page_config(page_title="IIoT Equipment Health Monitor", layout="wide")

st.title("🛢️ IIoT Equipment Health Monitoring Dashboard")
st.caption("Simulated real-time pump telemetry with anomaly detection")

REFRESH_SECONDS = 2

placeholder = st.empty()


def load_data():
    try:
        df = pd.read_csv(CSV_FILE)
    except FileNotFoundError:
        return None
    if df.empty:
        return None
    return detect_anomalies(df)


def render(df: pd.DataFrame):
    with placeholder.container():
        latest = df.iloc[-1]
        total_anomalies = int(df["is_anomaly_detected"].sum())

        col1, col2, col3, col4 = st.columns(4)
        col1.metric("Pressure (psi)", f"{latest['pressure']:.1f}")
        col2.metric("Temperature (F)", f"{latest['temperature']:.1f}")
        col3.metric("Vibration (mm/s)", f"{latest['vibration']:.2f}")
        col4.metric("Anomalies Detected", total_anomalies)

        if bool(latest["is_anomaly_detected"]):
            st.error(f"⚠️ Anomaly detected at {latest['timestamp']}!")
        else:
            st.success("✅ Equipment operating normally")

        st.subheader("Sensor Readings Over Time")
        chart_df = df.set_index("timestamp")[SENSOR_COLUMNS]
        st.line_chart(chart_df)

        st.subheader("Recent Anomalies")
        flagged = df[df["is_anomaly_detected"]].tail(10)
        if flagged.empty:
            st.write("No anomalies detected yet.")
        else:
            st.dataframe(
                flagged[["timestamp"] + SENSOR_COLUMNS],
                use_container_width=True,
            )


# --- Auto-refresh loop ---
df = load_data()
if df is None:
    st.warning(
        "No data found yet. Run `python sensor_simulator.py` in another "
        "terminal first, then this dashboard will pick up readings automatically."
    )
else:
    render(df)

time.sleep(REFRESH_SECONDS)
st.rerun()