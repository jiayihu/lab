const fs = require('fs');

const file = fs.readFileSync(
  './API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2_10181218/API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2_10181218.csv',
  'utf-8',
);
const europe = [
  'Albania',
  'Andorra',
  'Armenia',
  'Austria',
  'Azerbaijan',
  'Belarus',
  'Belgium',
  'Bosnia and Herzegovina',
  'Bulgaria',
  'Croatia',
  'Cyprus',
  'Czech Republic',
  'Denmark',
  'Estonia',
  'Finland',
  'France',
  'Georgia',
  'Germany',
  'Greece',
  'Hungary',
  'Iceland',
  'Ireland',
  'Italy',
  'Kosovo',
  'Latvia',
  'Liechtenstein',
  'Lithuania',
  'Luxembourg',
  'Macedonia',
  'Malta',
  'Moldova',
  'Monaco',
  'Montenegro',
  'The Netherlands',
  'Norway',
  'Poland',
  'Portugal',
  'Romania',
  'Russia',
  'San Marino',
  'Serbia',
  'Slovakia',
  'Slovenia',
  'Spain',
  'Sweden',
  'Switzerland',
  'Turkey',
  'Ukraine',
  'United Kingdom',
  'Vatican City',
];
const rows = file.replace(/"/g, '').split('\n');
const formatted = rows
  .filter(row => {
    const cells = row.split(',');
    const country = cells[0];

    return europe.includes(country);
  })
  .map(row => {
    const cells = row.split(',');
    const country = cells[0];
    const code = cells[1];
    const expenditure = cells[cells.length - 2]; // 2016

    return `${country},${code},${expenditure}`;
  });
fs.writeFileSync('./expenditure.csv', formatted.join('\n'));
