import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle, Ellipse

plt.rcParams["font.family"] = "DejaVu Sans"
plt.rcParams["svg.fonttype"] = "none"
fig, ax = plt.subplots(figsize=(12.2, 7.4))
ax.set_xlim(0, 12.2); ax.set_ylim(1.0, 8.4); ax.axis("off")

INK = "#222222"; RED = PUR = BLU = INK
X0, BW = 0.2, 3.3
XR = X0 + BW

def box(x, y, w, h, text, fc="white", ec=INK, lw=1.3, fs=10.5, bold=False):
    ax.add_patch(Rectangle((x, y - h/2), w, h, fc=fc, ec=ec, lw=lw, zorder=3))
    ax.text(x + w/2, y, text, ha="center", va="center", fontsize=fs,
            fontweight="bold" if bold else "normal", zorder=4, linespacing=1.15)

def arrow(p0, p1, color=INK, ls="-", lw=1.6, ms=13, head="-|>"):
    ax.annotate("", xy=p1, xytext=p0, zorder=2,
                arrowprops=dict(arrowstyle=head, color=color, lw=lw, ls=ls, shrinkA=0, shrinkB=0, mutation_scale=ms))

def sign(p0, p1, t, text, color=INK, fs=15):
    x = p0[0] + t*(p1[0]-p0[0]); y = p0[1] + t*(p1[1]-p0[1])
    ax.text(x, y, text, fontsize=fs, color=color, ha="center", va="center", zorder=5, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.12", fc="white", ec="none"))

# ---------------- izquierda ----------------
ax.text(X0, 8.1, "CONTEXTO", fontsize=11.5, fontweight="bold", va="center")
ctx = [("C1", "Estatus promedio", 7.55),
       ("C2", "Clima meritocrático", 6.95),
       ("C3", "Homogeneidad socioeconómica", 6.35),
       ("C4", "Rendimiento promedio", 5.75)]
ind = [("A3", "Capital cultural", 4.45),
       ("A2", "SES", 3.79),
       ("A1", "Justicia en las notas", 3.13),
       ("A4", "Rendimiento (promedio de notas)", 2.47),
       ("A5", "Creencia en la\nmeritocracia escolar", 1.81)]
pos = {}
for k, t, y in ctx + ind:
    box(X0, y, BW, 0.54 if "\n" in t else 0.42, t)
    pos[k] = (XR, y)
ax.plot([X0 - 0.1, XR + 0.1], [5.25, 5.25], color=INK, lw=2.2, zorder=2)
ax.text(X0, 5.02, "NIVEL INDIVIDUAL", fontsize=11.5, fontweight="bold", va="center")

# ---------------- factor + indicadores ----------------
CX, CY, R = 7.1, 3.9, 1.05                    # círculo pequeño
ax.add_patch(plt.Circle((CX, CY), R, fc="white", ec=INK, lw=2.4, zorder=1))
ax.text(CX, CY, "Percepción\nde recompensa\nen la sociedad", ha="center", va="center",
        fontsize=10.5, fontweight="bold", color=INK, zorder=4, linespacing=1.2)
IX0, IW, IH = 9.75, 2.25, 0.95
IT, IE = 5.0, 2.8
box(IX0, IT, IW, IH, "TALENTO\nes recompensado\n(p1_2)", fc="white", ec=INK, lw=2.0, fs=11, bold=True)
box(IX0, IE, IW, IH, "ESFUERZO\nes recompensado\n(p1_1)", fc="white", ec=INK, lw=2.0, fs=11, bold=True)
TARGET = (CX - R, CY)                         # punto único de convergencia
def edge_toward(p0):
    return TARGET
def circ(theta):
    th = np.radians(theta); return (CX + R*np.cos(th), CY + R*np.sin(th))
arrow(circ(30), (IX0, IT), color=INK, lw=1.5)
arrow(circ(-30), (IX0, IE), color=INK, lw=1.5)

# ---------------- efectos principales (solo signos) ----------------
eff = [("C1", "+", 0.42), ("C2", "+", 0.42), ("C3", "+", 0.42),
       ("A3", "+", 0.42), ("A2", "+ / −", 0.42), ("A1", "+", 0.42),
       ("A4", "+", 0.42), ("A5", "+", 0.42)]
ends = {}
for k, sg, t in eff:
    p0 = pos[k]; p1 = edge_toward(p0); ends[k] = (p0, p1)
    arrow(p0, p1, color=INK)
    sign(p0, p1, t, sg, color=INK)

fig.savefig("esquema_hipotesis.png", dpi=170, bbox_inches="tight", facecolor="white")
fig.savefig("esquema_hipotesis.svg", bbox_inches="tight", facecolor="white")
