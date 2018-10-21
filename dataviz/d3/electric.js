(async function() {
  const zip = (as, bs) => as.map((a, i) => [a, bs[i]]);
  const zip4 = (as, bs, cs, ds) => as.map((a, i) => [a, bs[i], cs[i], ds[i]]);
  const parseTime = d3.timeParse('%Y-%m');
  const formatTime = d3.timeFormat('%b %Y');

  const response = await d3.text('vehicle_sales_data.csv');
  const [[_, ...makes], [__, ...models], [___, ...types], ...years] = d3.csvParseRows(response);

  const vehicleData = years.map(([date, ...yearSales]) => {
    const salesByMakeModel = zip4(makes, models, types, yearSales).reduce(
      (map, [make, model, type, sales]) => {
        map[`${make} ${model}`] = { make, model, type, sales: sales ? Number(sales) : 0 };
        return map;
      },
      {},
    );

    return {
      date: parseTime(date),
      ...salesByMakeModel,
    };
  });
  const typeData = years.map(([date, ...yearSales]) => {
    const salesByType = zip(types, yearSales).reduce(
      (map, [type, sales]) => {
        map[type] += sales ? Number(sales) : 0;
        return map;
      },
      {
        HEV: 0,
        PHEV: 0,
        BEV: 0,
        FCEV: 0,
      },
    );

    return {
      date: parseTime(date),
      ...salesByType,
    };
  });

  const vehicleKeys = Object.keys(vehicleData[0]).filter(x => x !== 'date');
  const vehicleStack = d3
    .stack()
    // .order(d3.stackOrderDescending)
    .keys(vehicleKeys)
    .value((d, key) => d[key].sales);
  const vehicleSeries = vehicleStack(vehicleData);

  const typeStack = d3.stack().keys(['HEV', 'PHEV', 'BEV', 'FCEV']);
  const typeSeries = typeStack(typeData);

  const key = d => d.key;

  /**
   * Mike Bostock's margin convention
   * @see {@link https://bl.ocks.org/mbostock/3019563}
   */
  const margin = { top: 20, right: 40, bottom: 20, left: 30 };
  const width = 800 - margin.left - margin.right;
  const height = 400 - margin.top - margin.bottom;
  const svg = d3
    .select('.electric')
    .append('svg')
    .style('width', width + margin.left + margin.right)
    .style('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left}, ${margin.top})`);

  const xScale = d3
    .scaleTime()
    .domain([d3.min(vehicleData, d => d.date), d3.max(vehicleData, d => d.date)])
    .range([0, width]);
  const yScale = d3
    .scaleLinear()
    .domain([0, d3.max(vehicleData, d => vehicleKeys.reduce((sum, key) => sum + d[key].sales, 0))])
    .range([height, 0])
    .nice();

  const xAxis = d3
    .axisBottom()
    .scale(xScale)
    .ticks(10)
    .tickFormat(formatTime);
  const yAxis = d3
    .axisRight()
    .scale(yScale)
    .ticks(5);

  const area = d3
    .area()
    .x(d => xScale(d.data.date))
    .y0(d => yScale(d[0]))
    .y1(d => yScale(d[1]));

  svg
    .append('g')
    .attr('id', 'vehicles')
    .selectAll('path')
    .data(vehicleSeries, key)
    .enter()
    .append('path')
    .attr('class', 'area')
    .attr('opacity', 0)
    .attr('d', area)
    .attr('fill', d => {
      const type = d[0].data[d.key].type;

      switch (type) {
        case 'HEV':
          return d3.schemeCategory10[0];
        case 'PHEV':
          return d3.schemeCategory10[1];
        case 'BEV':
          return d3.schemeCategory10[2];
        case 'FCEV':
          return d3.schemeCategory10[3];
      }
    })
    .append('title')
    .text(d => d.key);

  const typesChart = svg.append('g').attr('id', 'types');

  typesChart
    .selectAll('path')
    .data(typeSeries, key)
    .enter()
    .append('path')
    .attr('class', 'area')
    .attr('opacity', 1)
    .attr('d', area)
    .attr('fill', d => {
      const type = d.key;

      switch (type) {
        case 'HEV':
          return 'rgb(110, 64, 170)';
        case 'PHEV':
          return 'rgb(76, 110, 219)';
        case 'BEV':
          return 'rgb(35, 171, 216)';
        case 'FCEV':
          return 'rgb(29, 223, 163)';
      }
    })
    .on('click', function(d) {
      const type = d.key;
      const clickedTypeData = typeData.map(x => {
        return {
          date: x.date,
          HEV: 0,
          PHEV: 0,
          BEV: 0,
          FCEV: 0,
          [type]: x[type],
        };
      });
      const clickedTypeSeries = typeStack(clickedTypeData);
      const paths = d3.selectAll('#types path').data(clickedTypeSeries, key);
      const areaTransitions = paths
        .transition()
        .duration(1000)
        .attr('d', area);

      yScale.domain([0, d3.max(clickedTypeData, d => d[type])]);

      areaTransitions
        .transition()
        .delay(200)
        .duration(1000)
        .on('start', function() {
          d3.select('.axis.y')
            .transition()
            .duration(1000)
            .call(yAxis);
        })
        .attr('d', area);
    })
    .append('title')
    .text(d => d.key);

  svg
    .append('g')
    .attr('class', 'axis x')
    .attr('transform', `translate(0, ${height})`)
    .call(xAxis);

  svg
    .append('g')
    .attr('class', 'axis y')
    .attr('transform', `translate(${width}, 0)`)
    .call(yAxis);
})();
