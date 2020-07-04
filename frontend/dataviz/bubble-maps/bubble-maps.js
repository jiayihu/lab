;(async function() {
  const dataset = await d3.csv('hierarchical.csv', d => {
    return {
      x: Number(d.x),
      y: Number(d.y),
      population: Number(d.population),
      cluster: Number(d.cluster),
    }
  })

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 0, right: 0, bottom: 0, left: 0 }
  const width = 1000 - margin.left - margin.right
  const height = 634 - margin.top - margin.bottom
  const svg = d3
    .select('.bubble-maps')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`)

  const xScale = d3
    .scaleLinear()
    .domain([0, 1000])
    .range([0, width])
  const yScale = d3
    .scaleLinear()
    .domain([0, 634])
    .range([0, height])
  const radiusScale = d3
    .scaleSqrt()
    .domain([0, d3.max(dataset, d => d.population)])
    .range([0, 20])
  const designedColors = [
    '#267278',
    '#65338d',
    '#4770b3',
    '#d21f75',
    '#3b3689',
    '#50aed3',
    '#48b24f',
    '#e57438',
    '#569dd2',
    '#569d79',
    '#58595b',
    '#e4b031',
    '#84d2f4',
    '#cad93f',
    '#f5c8af',
    '#9ac483',
    '#9e9ea2',
  ]

  svg
    .selectAll('circle')
    .data(dataset)
    .enter()
    .append('circle')
    .attr('cx', d => xScale(d.x))
    .attr('cy', d => yScale(d.y))
    .attr('r', d => radiusScale(d.population))
    .attr('fill', d => designedColors[d.cluster])
    .attr('fill-opacity', 0.5)
    .attr('stroke', d => designedColors[d.cluster])
})()
