"""
Painting to Sound
------------------
Converts a painting (image) into an ambient audio soundscape.

How it works:
- The image is scanned left to right (like reading a timeline).
- Each vertical slice (column) of the painting is sampled at several
  points from top to bottom.
- Each sample point becomes a sine tone:
    - Hue (color)      -> pitch/frequency
    - Brightness       -> volume
    - Saturation       -> how "rich"/textured the tone is (adds harmonics)
- All tones for a column play together, then the next column plays,
  sweeping across the painting like a slow scan, producing an evolving
  ambient soundscape that reflects the painting's colors and composition.

Usage:
    python painting_to_sound.py <path_to_image> [output.wav] [duration_seconds]

Example:
    python painting_to_sound.py sample_painting.png output.wav 20
"""

import sys
import colorsys
import numpy as np
from PIL import Image
from scipy.io.wavfile import write

SAMPLE_RATE = 44100


def load_image(path, target_width=120, target_height=40):
    """Load and resize image so we have a manageable grid of samples."""
    img = Image.open(path).convert("RGB")
    img = img.resize((target_width, target_height))
    return np.array(img) / 255.0  # normalize to 0-1


def hue_to_frequency(hue, low=110, high=880):
    """Map a hue (0-1) to a musical-ish frequency range (log scale)."""
    # Using a log scale so the frequency mapping feels more musical
    return low * (high / low) ** hue


def make_tone(freq, amplitude, duration, saturation):
    """Generate one sine tone, adding light harmonics based on saturation."""
    t = np.linspace(0, duration, int(SAMPLE_RATE * duration), False)
    wave = np.sin(2 * np.pi * freq * t)

    # Saturation adds a touch of overtone (2nd harmonic) for "richer" color
    if saturation > 0.15:
        wave += 0.3 * saturation * np.sin(2 * np.pi * freq * 2 * t)

    # Simple fade in/out per tone to avoid clicking
    fade_len = max(1, int(0.1 * len(t)))
    envelope = np.ones_like(t)
    envelope[:fade_len] = np.linspace(0, 1, fade_len)
    envelope[-fade_len:] = np.linspace(1, 0, fade_len)

    return wave * envelope * amplitude


def painting_to_sound(image_path, output_path="output.wav", duration=20):
    pixels = load_image(image_path)
    height, width, _ = pixels.shape

    col_duration = duration / width
    full_audio = np.zeros(int(SAMPLE_RATE * duration) + SAMPLE_RATE)  # padded

    position = 0
    for col in range(width):
        column_pixels = pixels[:, col, :]  # all rows for this column
        column_audio = np.zeros(int(SAMPLE_RATE * col_duration))

        # Sample a handful of rows per column (not every single pixel,
        # to keep the sound from being too dense/muddy)
        sample_rows = np.linspace(0, height - 1, 6, dtype=int)

        for row in sample_rows:
            r, g, b = column_pixels[row]
            hue, sat, val = colorsys.rgb_to_hsv(r, g, b)

            # top of painting -> higher pitch, bottom -> lower pitch
            pitch_bias = 1 - (row / height)
            freq = hue_to_frequency(hue) * (0.6 + 0.8 * pitch_bias)

            amplitude = 0.15 * val  # brightness -> volume
            tone = make_tone(freq, amplitude, col_duration, sat)
            column_audio[:len(tone)] += tone

        end = position + len(column_audio)
        full_audio[position:end] += column_audio
        position += len(column_audio)

    full_audio = full_audio[:position]

    # Normalize to avoid clipping
    max_val = np.max(np.abs(full_audio))
    if max_val > 0:
        full_audio = full_audio / max_val * 0.9

    audio_int16 = (full_audio * 32767).astype(np.int16)
    write(output_path, SAMPLE_RATE, audio_int16)
    print(f"Saved: {output_path} ({duration}s)")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python painting_to_sound.py <image_path> [output.wav] [duration_seconds]")
        sys.exit(1)

    image_path = sys.argv[1]
    output_path = sys.argv[2] if len(sys.argv) > 2 else "output.wav"
    duration = float(sys.argv[3]) if len(sys.argv) > 3 else 20

    painting_to_sound(image_path, output_path, duration)