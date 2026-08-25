"""
sensor_simulator.py

Simulates readings from an oilfield equipment sensor (e.g. a pump).
Generates: pressure (psi), temperature (F), vibration (mm/s)

Normal readings drift gently around a baseline.
Occasionally (controlled by ANOMALY_CHANCE) a reading spikes to simulate
equipment stress / failure conditions - this is what your anomaly
detector will later learn to catch.

Run this script and it will continuously append rows to sensor_data.csv,
one reading every INTERVAL_SECONDS, until you stop it (Ctrl+C).
"""

import csv
import os
import random
import time
from datetime import datetime

CSV_FILE = "sensor_data.csv"
INTERVAL_SECONDS = 1          # how often to generate a new reading
ANOMALY_CHANCE = 0.05         # 5% chance any given reading is an anomaly

# "Normal" operating baselines for a simulated pump
BASELINE = {
    "pressure": 100.0,    # psi
    "temperature": 150.0, # Fahrenheit
    "vibration": 2.0,     # mm/s
}

# How much normal readings are allowed to wobble
NOISE = {
    "pressure": 3.0,
    "temperature": 2.0,
    "vibration": 0.3,
}


def generate_reading():
    """Generate one sensor reading, occasionally injecting an anomaly."""
    is_anomaly = random.random() < ANOMALY_CHANCE

    if is_anomaly:
        # Pick one sensor to spike hard - simulates a real equipment fault
        faulty_sensor = random.choice(list(BASELINE.keys()))
        reading = {
            key: round(random.gauss(BASELINE[key], NOISE[key]), 2)
            for key in BASELINE
        }
        # Push the faulty sensor 4-8x its normal noise band away from baseline
        spike_direction = random.choice([1, -1])
        reading[faulty_sensor] = round(
            BASELINE[faulty_sensor]
            + spike_direction * NOISE[faulty_sensor] * random.uniform(5, 9),
            2,
        )
    else:
        reading = {
            key: round(random.gauss(BASELINE[key], NOISE[key]), 2)
            for key in BASELINE
        }

    reading["timestamp"] = datetime.now().isoformat(timespec="seconds")
    reading["is_anomaly_injected"] = is_anomaly  # ground truth, for your own testing
    return reading


def main():
    file_exists = os.path.isfile(CSV_FILE)

    fieldnames = ["timestamp", "pressure", "temperature", "vibration", "is_anomaly_injected"]

    with open(CSV_FILE, "a", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        if not file_exists:
            writer.writeheader()

        print(f"Simulating sensor data -> {CSV_FILE} (Ctrl+C to stop)")
        try:
            while True:
                reading = generate_reading()
                writer.writerow(reading)
                f.flush()  # make sure it's written immediately so the dashboard can read it

                flag = " <-- ANOMALY" if reading["is_anomaly_injected"] else ""
                print(
                    f"{reading['timestamp']} | "
                    f"P={reading['pressure']:6.2f} psi  "
                    f"T={reading['temperature']:6.2f} F  "
                    f"V={reading['vibration']:5.2f} mm/s"
                    f"{flag}"
                )
                time.sleep(INTERVAL_SECONDS)
        except KeyboardInterrupt:
            print("\nStopped.")


if __name__ == "__main__":
    main()