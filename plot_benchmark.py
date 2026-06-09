import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

# ---- load & average the 10 runs ----
runs = pd.concat([pd.read_csv(f'run{i}.csv') for i in range(1, 11)])
data = runs.groupby(
    ['image', 'filter', 'approach', 'threads'])['time_ms'].mean().reset_index()

images  = ['cat', 'cats2', 'new-york']
img_lbl = {
    'cat':      'cat.png  (34K px)',
    'cats2':    'cats2.png  (1M px)',
    'new-york': 'new-york  (5.7M px)',
}
filters = ['grayscale', 'sepia', 'negative']
threads = [1, 2, 4, 8, 16]

# ---- compute speedup & efficiency ----
# S(n) = T_seq / T(n)       where T_seq = sequential functional (sf)
# E(n) = S(n) / n
rows = []
for img in images:
    for filt in filters:
        d = data[(data['image'] == img) & (data['filter'] == filt)]
        t_seq = d[(d['approach'] == 'sf') & (d['threads'] == 0)]['time_ms'].values[0]
        for n in threads:
            for appr in ('pf', 'pm'):
                t_par = d[(d['approach'] == appr) & (d['threads'] == n)]['time_ms'].values
                if len(t_par) == 0:
                    continue
                s = t_seq / t_par[0]
                rows.append(dict(image=img, filter=filt, approach=appr,
                                 threads=n, speedup=s, efficiency=s / n))
df = pd.DataFrame(rows)

# ---- plot ----
colors  = {'pf': '#4878d0', 'pm': '#d65f5f'}
markers = {'pf': 'o',       'pm': 'D'}
names   = {'pf': 'par funcional', 'pm': 'par mixto'}

fig, axes = plt.subplots(2, 3, figsize=(14, 8))
fig.suptitle(
    'Speedup  y  Eficiencia — Funcional vs Mixto\n'
    r'$S(n)=T_1/T_n \quad E(n)=S(n)/n$',
    fontsize=13, fontweight='bold')

for j, img in enumerate(images):
    for row_i, metric in enumerate(('speedup', 'efficiency')):
        ax = axes[row_i][j]

        # ideal reference
        if metric == 'speedup':
            ax.plot(threads, threads, 'k--', lw=1, alpha=0.35, label='ideal')
        else:
            ax.axhline(1.0, color='k', linestyle='--', lw=1, alpha=0.35, label='ideal (E=1)')

        # thin lines per filter (background detail)
        for filt in filters:
            for appr in ('pf', 'pm'):
                fd = df[(df['image'] == img) & (df['filter'] == filt) &
                        (df['approach'] == appr)].sort_values('threads')
                ax.plot(fd['threads'], fd[metric],
                        color=colors[appr], alpha=0.18, lw=1.2)

        # bold average line per approach
        for appr in ('pf', 'pm'):
            avg = (df[(df['image'] == img) & (df['approach'] == appr)]
                   .groupby('threads')[metric].mean()
                   .reindex(threads))
            ax.plot(threads, avg, markers[appr] + '-',
                    color=colors[appr], lw=2.5, ms=6, label=names[appr])

            # annotate max value
            peak = avg.max()
            peak_n = avg.idxmax()
            ax.annotate(f'{peak:.2f}',
                        xy=(peak_n, peak),
                        xytext=(4, 4), textcoords='offset points',
                        fontsize=7, color=colors[appr])

        ax.set_xscale('log', base=2)
        ax.set_xticks(threads)
        ax.get_xaxis().set_major_formatter(ticker.ScalarFormatter())
        ax.grid(True, alpha=0.25, linestyle=':')

        if row_i == 0:
            ax.set_title(img_lbl[img], fontsize=10, fontweight='bold')
            ax.set_ylabel('Speedup  S(n)', fontsize=9)
            ax.set_ylim(bottom=0)
        else:
            ax.set_ylabel('Eficiencia  E(n)', fontsize=9)
            ax.set_xlabel('Hilos  (n)', fontsize=9)
            ax.set_ylim(0, 1.3)
            ax.axhspan(0, 0.5,  alpha=0.04, color='red')
            ax.axhspan(0.5, 0.75, alpha=0.04, color='orange')
            ax.axhspan(0.75, 1.3, alpha=0.04, color='green')

        if row_i == 0 and j == 0:
            ax.legend(fontsize=8, loc='upper left')

plt.tight_layout(rect=[0, 0, 1, 0.94])
plt.savefig('speedup_efficiency.png', dpi=150, bbox_inches='tight')
print('Guardado: speedup_efficiency.png')
