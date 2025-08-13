# pmplot

## Overview
**pmplot** provides functions for adding clean, publication-ready brackets to **ggplot2** plots.

- Supports top, bottom, left, and right brackets in normalized panel coordinates (0–1).
- Works with facets, multiple brackets per plot, stacked arrangements, and off-panel labels.
- Default text style: Arial, 10 pt, plain.

## Dependencies
- **ggplot2**
- **grid**
- **patchwork** (for multi-panel / shared legends)

Install:
```r
install.packages(c("ggplot2", "patchwork", "grid"))
## 🐧 Penguins Demo

The **pmplot** package can be demonstrated using the [`palmerpenguins`] dataset.  
These examples show how to add horizontal and vertical brackets in various layouts, handle legends, and combine panels into a final publication-ready figure.

---

### **P1 — Mixed Horizontals + Verticals (Scatter)**  
- **Data**: Bill length vs flipper length, colored by species.  
- **Brackets**:  
  - Two **top** bands: "Short Bills", "Long Bills"  
  - Two **right** bands: "Short Flippers", "Long Flippers"  
- **Legend**: Nudged far right using:
  ```r
  theme(
    legend.position = c(1.15, 0.5),
    legend.justification = c("left", "center"),
    plot.margin = margin(t = 40, r = 150, b = 0, l = 0)
  )
![P1](R/example%20and%20plot/p1.png)
