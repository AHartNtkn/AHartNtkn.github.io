---
layout: page
title: Toy SPEEDEX
subtitle: Triangle-Edge Market Visualization
---

## About Toy SPEEDEX

This is a toy interactive simulation of a simple market using an idealized version of Tâtonnement to calculate the equilibrium price inside a market of limit orders.

This visualization is intended to build intuition through a unified, interactive geometric representation. It shows a toy market with three assets, named "A", "B", and "C". Orders are organized along the edges of the triangle, with exterior orders trading assets clockwise and interior orders trading assets counter-clockwise. The position of the limit order along the edge indicates the limit trade ratio expected by the order, and the size of the order's bar indicates how much its trading. The three price ratios are collectively represented by the position of the bead which moves around. At each asset label is a reservoir which fills up with either excess supply or excess demand. At the bead is an arrow indicating the direction the bead must move in order to make supply and demand more balanced. You may press play in the top-right corner to see the price evolve until it eventually settles at an equilibrium point. If the market is not too awful, it will find a point where the excess supply and demand are everywhere zero. Depending on the price ratios, orders will execute or not, and colors change to accommodate. You may click and drag the bead to manually set the price. You may also manually click and drag orders to set their ratios and trading volumes. Press the "+" buttons to add new orders. And press the trash can button to remove selected orders.

## Interactive Visualization

<div style="width: 100%; height: 70vh; border: 1px solid #ccc; margin: 20px 0;">
<iframe src="{{ '/assets/interactive/speedex.html' | relative_url }}" 
        width="100%" 
        height="100%" 
        frameborder="0" 
        allowfullscreen>
    Your browser does not support iframes. Please <a href="{{ '/assets/interactive/speedex.html' | relative_url }}">click here</a> to view the SPEEDEX visualization directly.
</iframe>
</div>

## Research Paper

For a detailed technical explanation of SPEEDEX and its theoretical foundations, you can read the full research paper:

**[SPEEDEX: A Scalable, Parallelizable, and Economically Efficient Decentralized Exchange](https://arxiv.org/abs/2111.02719)**

*Note: If you're having trouble viewing the interactive visualization above, you can also [access it directly]({{ '/assets/interactive/speedex.html' | relative_url }}) in a new window.*
