(async function() {
  const dataset = await d3.csv('mauna_loa_co2_monthly_averages.csv', d => {
    return {
      date: new Date(Number(d.year), Number(d.month) + 1),
      average: Number(d.average)
    };
  });

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 600 - margin.left - margin.right;
  const height = 250 - margin.top - margin.bottom;
  const svg = d3
    .select('.paths')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const xScale = d3
    .scaleTime()
    .domain([d3.min(dataset, d => d.date), d3.max(dataset, d => d.date)])
    .range([0, width]);
  const yScale = d3
    .scaleLinear()
    .domain([300, d3.max(dataset, d => d.average)])
    .range([height, 0]);

  const line = d3
    .line()
    .defined(d => d.average >= 0 && d.average <= 350)
    .x(d => xScale(d.date))
    .y(d => yScale(d.average));

  const dangerLine = d3
    .line()
    .defined(d => d.average > 350)
    .x(d => xScale(d.date))
    .y(d => yScale(d.average));

  svg
    .append('path')
    .datum(dataset)
    .classed('line', true)
    .attr('d', line)
    .attr('fill', 'none')
    .attr('stroke', 'teal');
  svg
    .append('path')
    .datum(dataset)
    .classed('line', true)
    .attr('d', dangerLine)
    .attr('fill', 'none')
    .attr('stroke', 'red');

  svg
    .append('line')
    .attr('x1', 0)
    .attr('y1', yScale(350))
    .attr('x2', width)
    .attr('y2', yScale(350))
    .attr('stroke', 'red')
    .attr('stroke-dasharray', 5);

  svg
    .append('text')
    .attr('x', 0)
    .attr('y', yScale(351))
    .text('350 ppm safe level')
    .style('stroke', 'red')
    .style('font-size', '14px')
    .style('fill', 'none');

  const xAxis = d3.axisBottom(xScale);
  svg
    .append('g')
    .classed('axis', true)
    .attr('transform', `translate(0, ${height})`)
    .call(xAxis);

  const yAxis = d3.axisLeft(yScale);
  svg
    .append('g')
    .classed('axis', true)
    .call(yAxis);
})();
