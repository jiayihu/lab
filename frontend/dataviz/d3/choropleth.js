(async function() {
  const [dataAgric, dataStates, dataCities] = await Promise.all([
    d3.csv('us-ag-productivity.csv', d => {
      return { state: d.state, value: Number(d.value) };
    }),
    d3.json('us-states.json'),
    d3.csv('us-cities.csv', d => {
      return {
        rank: Number(d.rank),
        place: d.place,
        population: Number(d.population),
        lat: Number(d.lat),
        lon: Number(d.lon),
      };
    }),
  ]);

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 900 - margin.left - margin.right;
  const height = 500 - margin.top - margin.bottom;
  const svg = d3
    .select('.choropleth')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  svg
    .append('clipPath')
    .attr('id', 'chart-area')
    .append('rect')
    .attr('x', 0)
    .attr('y', 0)
    .attr('width', width)
    .attr('height', height);

  const colorScale = d3
    .scaleQuantize()
    .domain([d3.min(dataAgric, d => d.value), d3.max(dataAgric, d => d.value)])
    .range(['#fef0d9', '#fdcc8a', '#fc8d59', '#e34a33', '#b30000']);
  const radiusScale = d3
    .scaleSqrt()
    .domain([d3.min(dataCities, d => d.population), d3.max(dataCities, d => d.population)])
    .range([4, 20]);

  dataAgric.forEach(data => {
    const state = data.state;
    const value = data.value;
    const json = dataStates.features.find(x => x.properties.name === state);

    if (json) json.properties.value = value;
  });

  const scale = 2000;
  const projection = d3.geoAlbersUsa();
  const path = d3.geoPath().projection(projection);
  const formatAsThousands = d3.format(',.2r');

  const zoom = d3
    .zoom()
    .scaleExtent([0.2, 2])
    .translateExtent([[-1200, -700], [1200, 700]])
    .on('zoom', function() {
      const offset = [d3.event.transform.x, d3.event.transform.y];
      const newScale = d3.event.transform.k * scale;

      projection.translate(offset).scale(newScale);
      svg.selectAll('path').attr('d', path);
      svg
        .selectAll('circle')
        .attr('cx', d => projection([d.lon, d.lat])[0])
        .attr('cy', d => projection([d.lon, d.lat])[1]);
    });

  const map = svg
    .append('g')
    .attr('id', 'map')
    .attr('clip-path', 'url(#chart-area)')
    .call(zoom)
    .call(zoom.transform, d3.zoomIdentity.translate(width / 2, height / 2).scale(0.5));

  // Phantom rect to allow dragging where there is no path
  map
    .append('rect')
    .attr('x', 0)
    .attr('y', 0)
    .attr('width', width)
    .attr('height', height)
    .style('opacity', 0);

  map
    .selectAll('path')
    .data(dataStates.features)
    .enter()
    .append('path')
    .attr('d', path)
    .attr('fill', d => {
      const value = d.properties.value;

      return value ? colorScale(value) : '#ccc';
    })
    .attr('stroke', 'rgba(255,255,255,.1)')
    .attr('stroke-width', 1);

  map
    .selectAll('circle')
    .data(dataCities)
    .enter()
    .append('circle')
    .attr('cx', d => projection([d.lon, d.lat])[0])
    .attr('cy', d => projection([d.lon, d.lat])[1])
    .attr('r', d => radiusScale(d.population))
    .style('fill', 'yellow')
    .style('opacity', 0.75)
    .style('stroke', 'gray')
    .style('stroke-width', 0.25)
    .append('title')
    .text(d => `${d.place}: Pop. ${formatAsThousands(d.population)}`);

  function handlePanClick() {
    const offset = [0, 0];
    const moveAmount = 100;
    const direction = d3.select(this).attr('id');

    switch (direction) {
      case 'north':
        offset[1] += moveAmount;
        break;
      case 'east':
        offset[0] -= moveAmount;
        break;
      case 'south':
        offset[1] -= moveAmount;
        break;
      case 'west':
        offset[0] += moveAmount;
        break;
      default:
        break;
    }

    map.transition().call(zoom.translateBy, offset[0], offset[1]);
  }

  const north = svg
    .append('g')
    .classed('pan', true)
    .attr('id', 'north')
    .on('click', handlePanClick);
  north
    .append('rect')
    .attr('x', 0)
    .attr('y', 0)
    .attr('width', width)
    .attr('height', 30)
    .attr('fill', 'aliceblue')
    .style('cursor', 'pointer');
  north
    .append('text')
    .attr('x', width / 2)
    .attr('y', 20)
    .text('↑')
    .style('pointer-events', 'none');

  const east = svg
    .append('g')
    .classed('pan', true)
    .attr('id', 'east')
    .on('click', handlePanClick);
  east
    .append('rect')
    .attr('x', width - 30)
    .attr('y', 0)
    .attr('width', 30)
    .attr('height', height)
    .attr('fill', 'aliceblue')
    .style('cursor', 'pointer');
  east
    .append('text')
    .attr('x', width - 20)
    .attr('y', height / 2)
    .text('→')
    .style('pointer-events', 'none');

  const south = svg
    .append('g')
    .classed('pan', true)
    .attr('id', 'south')
    .on('click', handlePanClick);
  south
    .append('rect')
    .attr('x', 0)
    .attr('y', height - 30)
    .attr('width', width)
    .attr('height', 30)
    .attr('fill', 'aliceblue')
    .style('cursor', 'pointer');
  south
    .append('text')
    .attr('x', width / 2)
    .attr('y', height - 10)
    .text('↓')
    .style('pointer-events', 'none');

  const west = svg
    .append('g')
    .classed('pan', true)
    .attr('id', 'west')
    .on('click', handlePanClick);
  west
    .append('rect')
    .attr('x', 0)
    .attr('y', 0)
    .attr('width', 30)
    .attr('height', height)
    .attr('fill', 'aliceblue')
    .style('cursor', 'pointer');
  west
    .append('text')
    .attr('x', 10)
    .attr('y', height / 2)
    .text('←')
    .style('pointer-events', 'none');

  const zoomy = svg.append('g').classed('zoomy', true);

  function handleZoomClick() {
    const direction = d3.select(this).attr('id');
    const scaleFactor = direction === 'zoom-in' ? 1.25 : 0.75;

    map.transition().call(zoom.scaleBy, scaleFactor);
  }

  zoomy
    .append('rect')
    .attr('id', 'zoom-in')
    .attr('x', width - 30 - 80)
    .attr('y', height - 30 - 40)
    .attr('width', 30)
    .attr('height', 30)
    .attr('fill', 'aliceblue')
    .style('cursor', 'pointer')
    .on('click', handleZoomClick);
  zoomy
    .append('text')
    .attr('x', width - 30 - 70)
    .attr('y', height - 30 - 20)
    .text('+')
    .style('pointer-events', 'none');

  zoomy
    .append('rect')
    .attr('id', 'zoom-out')
    .attr('x', width - 30 - 40)
    .attr('y', height - 30 - 40)
    .attr('width', 30)
    .attr('height', 30)
    .attr('fill', 'aliceblue')
    .style('cursor', 'pointer')
    .on('click', handleZoomClick);
  zoomy
    .append('text')
    .attr('x', width - 30 - 30)
    .attr('y', height - 30 - 20)
    .text('-')
    .style('pointer-events', 'none');
})();
