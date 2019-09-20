const fs = require('fs')

fs.readFile('time.txt', 'utf-8', (err, data) => {
    const times = data.split('\n')
        .filter(line => line.startsWith('Elapsed time:'))
        .map(line => Number(line.match(/\d\.\d+/)[0]))
        .slice(3)

    const median = times.reduce((sum, x) => sum + x, 0) / times.length;
    const dev = times.reduce((sum, x) => sum + Math.pow(x - median, 2), 0) / times.length;
    const sd = Math.sqrt(dev);

    console.log(median, sd, sd / median)
})