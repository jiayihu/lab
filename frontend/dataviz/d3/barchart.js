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

  const width = 500;
  const height = 120;
  const barPadding = 1;
  const svg = d3
    .select('.barchart')
    .append('svg')
    .attr('width', width)
    .attr('height', height);

  const rects = svg
    .selectAll('rect')
    .data(dataset)
    .enter()
    .append('rect');

  rects
    .attr('x', (d, i) => i * (width / dataset.length))
    .attr('y', d => height - d * 4)
    .attr('width', Math.min(width / dataset.length - barPadding, 20))
    .attr('height', d => d * 4)
    .style('fill', d => `rgb(${d * 10}, 0, 0)`);

  const texts = svg
    .selectAll('text')
    .data(dataset)
    .enter()
    .append('text');

  texts
    .text(d => d)
    .attr('x', (d, i) => i * (width / dataset.length) + width / dataset.length / 2)
    .attr('y', d => height - d * 4 + 14)
    .style('font-size', '12px')
    .style('fill', 'white')
    .style('text-anchor', 'middle');
})();
