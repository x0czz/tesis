import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

plt.rcParams["font.family"] = "DejaVu Sans"
plt.rcParams["svg.fonttype"] = "none"
INK = "#222222"
fig, ax = plt.subplots(figsize=(12.2, 6.6))
ax.set_xlim(0, 12.2); ax.set_ylim(0, 6.6); ax.axis("off")

def box(x, y, w, h, text, lw=1.6, fs=11, bold=False, ls="-"):
    ax.add_patch(Rectangle((x, y - h/2), w, h, fc="white", ec=INK, lw=lw, ls=ls, zorder=3))
    ax.text(x + w/2, y, text, ha="center", va="center", fontsize=fs, zorder=4,
            fontweight="bold" if bold else "normal", linespacing=1.2)

def arrow(p0, p1, lw=1.6, ls="-", head="-|>"):
    ax.annotate("", xy=p1, xytext=p0, zorder=2,
                arrowprops=dict(arrowstyle=head, color=INK, lw=lw, ls=ls, shrinkA=0, shrinkB=0, mutation_scale=15))

def lab(x, y, t, fs=12):
    ax.text(x, y, t, fontsize=fs, fontweight="bold", ha="center", va="center", zorder=5,
            bbox=dict(boxstyle="round,pad=0.15", fc="white", ec="none"))

# cajas principales
box(0.3, 3.4, 3.3, 1.3, "Percepción de recompensa\nen la ESCUELA\n(notas: esfuerzo, talento)", bold=True)
box(6.2, 3.4, 3.3, 1.3, "Percepción de recompensa\nen la SOCIEDAD\n(esfuerzo, talento)", bold=True)
box(10.4, 3.4, 1.7, 1.3, "Legitimación\nde la\ndesigualdad", ls=(0, (5, 3)), lw=1.3)
# H1 (arrow central grueso)
arrow((3.6, 3.4), (6.2, 3.4), lw=3.2)
lab(4.15, 3.75, "H1")
# efecto hacia legitimación (no contrastado)
arrow((9.5, 3.4), (10.4, 3.4), lw=1.3, ls=(0, (5, 3)))
# origen social (arriba)
box(1.6, 6.0, 3.9, 0.8, "Origen social\n(educación de los padres, libros)")
arrow((5.5, 5.9), (7.1, 4.05))
lab(6.55, 5.0, "H3, H4")
dot = (4.9, 3.4)
ax.plot([dot[0]], [dot[1]], "o", ms=7, color=INK, zorder=6)
arrow((3.55, 5.6), (4.88, 3.47), lw=1.3, ls=(0, (5, 3)), head="-")
lab(4.0, 4.7, "H2", fs=11)
# contexto (abajo)
box(1.6, 1.6, 3.9, 1.0, "Contexto del curso\n(estatus, clima,\nhomogeneidad, rendimiento)", fs=10.5)
arrow((5.5, 1.9), (7.1, 2.75))
lab(6.6, 2.15, "H7, H8, H10")
arrow((3.55, 2.1), (4.88, 3.33), lw=1.3, ls=(0, (5, 3)), head="-")
lab(4.0, 2.6, "H9", fs=11)
# cambio en el tiempo
ax.annotate("", xy=(9.6, 2.2), xytext=(0.3, 2.2), arrowprops=dict(arrowstyle="-", color="none"))
ax.text(6.2, 0.35, "Cambio entre olas (dentro de cada estudiante): H11, H12", ha="center", fontsize=11,
        bbox=dict(boxstyle="round,pad=0.35", fc="white", ec=INK, lw=1.1))
fig.savefig("modelo_conceptual.png", dpi=170, bbox_inches="tight", facecolor="white")
fig.savefig("modelo_conceptual.svg", bbox_inches="tight", facecolor="white")
