"""
ASCII Slot Machine - Windows GUI version (Tkinter)

HOW TO RUN ON WINDOWS:
  1. Open PowerShell / Command Prompt
  2. cd to the folder containing this file
  3. Run:  python Slot.py       (or:  py Slot.py)

You can also just double-click Slot.py in File Explorer if .py files
are associated with Python.

No extra installs needed - tkinter ships with the standard Windows
Python installer.
"""

import tkinter as tk
from tkinter import font as tkfont
import random

# ---------------- Game data ----------------

SYMBOLS = {
    "C": 8,   # Cherry
    "L": 7,   # Lemon
    "G": 6,   # Grape
    "B": 4,   # Bell
    "S": 3,   # Star
    "D": 2,   # Diamond
    "7": 1,   # Seven
}

PAYOUTS = {
    "C": 2,
    "L": 3,
    "G": 4,
    "B": 6,
    "S": 10,
    "D": 20,
    "7": 50,
}

PAIR_MULTIPLIER = 1
STARTING_BALANCE = 100

SYMBOL_NAMES = {
    "C": "Cherry",
    "L": "Lemon",
    "G": "Grape",
    "B": "Bell",
    "S": "Star",
    "D": "Diamond",
    "7": "Seven",
}


def weighted_symbol():
    symbols = list(SYMBOLS.keys())
    weights = list(SYMBOLS.values())
    return random.choices(symbols, weights=weights, k=1)[0]


def evaluate(reels, bet):
    a, b, c = reels
    if a == b == c:
        mult = PAYOUTS[a]
        return bet * mult, f"JACKPOT! Three {SYMBOL_NAMES[a]}s! x{mult} payout!"
    if a == b or b == c or a == c:
        return bet * PAIR_MULTIPLIER, f"Pair match! x{PAIR_MULTIPLIER} payout."
    return 0, "No match. Try again!"


# ---------------- GUI ----------------

class SlotMachineApp:
    def __init__(self, root):
        self.root = root
        self.root.title("ASCII Slot Machine")
        self.root.configure(bg="#1b1b2f")
        self.root.resizable(False, False)

        self.balance = STARTING_BALANCE
        self.reels = ["C", "C", "C"]
        self.spinning = False

        self.reel_font = tkfont.Font(family="Courier New", size=48, weight="bold")
        self.label_font = tkfont.Font(family="Courier New", size=14, weight="bold")
        self.msg_font = tkfont.Font(family="Courier New", size=12)

        # Title
        tk.Label(
            root, text="=== SLOT MACHINE ===", fg="#ffd700", bg="#1b1b2f",
            font=self.label_font
        ).pack(pady=(15, 5))

        # Reel display frame (the "machine body")
        frame = tk.Frame(root, bg="#3b2f2f", bd=6, relief="ridge")
        frame.pack(padx=20, pady=10)

        self.reel_labels = []
        reel_row = tk.Frame(frame, bg="#3b2f2f")
        reel_row.pack(padx=15, pady=15)
        for i in range(3):
            lbl = tk.Label(
                reel_row, text=self.reels[i], font=self.reel_font,
                fg="#ffffff", bg="#000000", width=2, relief="sunken", bd=4
            )
            lbl.grid(row=0, column=i, padx=8)
            self.reel_labels.append(lbl)

        # Balance
        self.balance_var = tk.StringVar(value=f"Balance: {self.balance}")
        tk.Label(
            root, textvariable=self.balance_var, fg="#00ff99", bg="#1b1b2f",
            font=self.label_font
        ).pack(pady=(5, 0))

        # Message line
        self.message_var = tk.StringVar(value="Place your bet and spin!")
        tk.Label(
            root, textvariable=self.message_var, fg="#ffffff", bg="#1b1b2f",
            font=self.msg_font, wraplength=320
        ).pack(pady=(5, 10))

        # Bet controls
        bet_frame = tk.Frame(root, bg="#1b1b2f")
        bet_frame.pack(pady=5)
        tk.Label(bet_frame, text="Bet:", fg="#ffffff", bg="#1b1b2f", font=self.msg_font).pack(side="left")
        self.bet_var = tk.StringVar(value="10")
        tk.Entry(bet_frame, textvariable=self.bet_var, width=6, font=self.msg_font).pack(side="left", padx=5)

        # Buttons
        btn_frame = tk.Frame(root, bg="#1b1b2f")
        btn_frame.pack(pady=10)
        self.spin_btn = tk.Button(
            btn_frame, text="SPIN", command=self.start_spin, bg="#e63946", fg="white",
            font=self.label_font, width=10, relief="raised", bd=4
        )
        self.spin_btn.grid(row=0, column=0, padx=5)

        tk.Button(
            btn_frame, text="Paytable", command=self.show_paytable, bg="#457b9d", fg="white",
            font=self.msg_font, width=10
        ).grid(row=0, column=1, padx=5)

    def show_paytable(self):
        lines = ["PAYTABLE (3 of a kind)\n"]
        for sym, mult in sorted(PAYOUTS.items(), key=lambda kv: kv[1]):
            lines.append(f"{SYMBOL_NAMES[sym]:8s} x{mult}")
        lines.append(f"\nAny pair   x{PAIR_MULTIPLIER}")
        self.message_var.set("\n".join(lines))

    def start_spin(self):
        if self.spinning:
            return
        raw = self.bet_var.get().strip()
        if not raw.isdigit():
            self.message_var.set("Enter a valid whole number bet.")
            return
        bet = int(raw)
        if bet <= 0:
            self.message_var.set("Bet must be greater than 0.")
            return
        if bet > self.balance:
            self.message_var.set("Not enough balance for that bet.")
            return

        self.spinning = True
        self.spin_btn.config(state="disabled")
        self.balance -= bet
        self.balance_var.set(f"Balance: {self.balance}")
        self.message_var.set("Spinning...")

        self.final_reels = [weighted_symbol() for _ in range(3)]
        self._animate(step=0, bet=bet)

    def _animate(self, step, bet):
        # Total animation "ticks" before each reel locks in
        total_ticks = 18
        lock_at = [6, 12, 18]  # reel 0 locks after 6 ticks, reel 1 after 12, reel 2 after 18

        for i in range(3):
            if step < lock_at[i]:
                self.reel_labels[i].config(text=random.choice(list(SYMBOLS.keys())))
            else:
                self.reel_labels[i].config(text=self.final_reels[i])

        if step < total_ticks:
            self.root.after(60, self._animate, step + 1, bet)
        else:
            self.reels = self.final_reels
            winnings, msg = evaluate(self.reels, bet)
            self.balance += winnings
            self.balance_var.set(f"Balance: {self.balance}")
            self.message_var.set(f"{msg}  (Bet {bet} -> Won {winnings})")
            self.spinning = False
            self.spin_btn.config(state="normal")

            if self.balance <= 0:
                self.message_var.set("You're out of coins! Restart the app to play again.")
                self.spin_btn.config(state="disabled")


def main():
    root = tk.Tk()
    app = SlotMachineApp(root)
    root.mainloop()


if __name__ == "__main__":
    main()