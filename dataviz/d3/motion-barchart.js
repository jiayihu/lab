(function() {
  const dataset = [
    25,
    7,
    5,
    26,
    11,
    8,
    25,
    14,
    23,
    19,
    14,
    11,
    22,
    29,
    11,
    13,
    12,
    17,
    18,
    10,
    24,
    18,
    25,
    9,
    3
  ];

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 600 - margin.left - margin.right;
  const height = 250 - margin.top - margin.bottom;
  const svg = d3
    .select('.motion-barchart')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const xScale = d3
    .scaleBand()
    .domain(d3.range(dataset.length))
    .range([0, width])
    .round(true)
    .paddingInner(0.05);
  const yScale = d3
    .scaleLinear()
    .domain([0, d3.max(dataset)])
    .range([0, height]);

  function renderRects(rects) {
    rects
      .attr('x', (d, i) => xScale(i))
      .attr('y', d => height - yScale(d))
      .attr('width', xScale.bandwidth())
      .attr('height', d => yScale(d))
      .style('fill', d => `rgb(${d * 10}, 0, 0)`);
  }

  function renderTexts(texts) {
    texts
      .text(d => d)
      .attr('x', (d, i) => xScale(i) + xScale.bandwidth() / 2)
      .attr('y', d => height - yScale(d) + 14)
      .style('font-size', '12px')
      .style('fill', 'white')
      .style('text-anchor', 'middle');
  }

  renderRects(
    svg
      .selectAll('rect')
      .data(dataset)
      .enter()
      .append('rect')
  );

  renderTexts(
    svg
      .selectAll('text')
      .data(dataset)
      .enter()
      .append('text')
  );

  d3.select('.motion-barchart')
    .select('button')
    .on('click', () => {
      const length = Math.round(Math.random() * 5) + 20;
      const dataset = Array.from({ length }, () => Math.round(Math.random() * 25) + 1);

      xScale.domain(d3.range(length));
      yScale.domain([0, d3.max(dataset)]);

      const rects = svg.selectAll('rect').data(dataset);

      rects.exit().remove();
      renderRects(
        rects
          .enter()
          .append('rect')
          .merge(rects)
          .transition()
      );

      const texts = svg.selectAll('text').data(dataset);

      texts.exit().remove();
      renderTexts(
        texts
          .enter()
          .append('text')
          .merge(texts)
          .transition()
      );
    });
})();
