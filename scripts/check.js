process.on('unhandledRejection', up => { throw up })
const { execSync } = require('child_process')
const run = (cmd) => execSync(cmd, {stdio: [process.stdin, process.stdout, process.stderr]})

const builddir = '.asterius-work/build-check';

run(`scripts/hpack.sh`)
run(`ahc-cabal new-build exe:cli -O0 --builddir=${builddir}`)
run(`ahc-cabal new-build exe:web -O0 --builddir=${builddir}`)
