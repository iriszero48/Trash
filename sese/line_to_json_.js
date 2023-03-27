const { once } = require("node:events");
const fs = require("node:fs");
const readline = require("node:readline");

async function main() {
  const args = process.argv.slice(1);
  if (args.length != 3) {
    console.error(`args.length != 3 which args.length = ${args.length}`);
    console.log(`Usage: ${args[0]} prefix path`);
    process.exit(1);
  }

  const prefix = args[1];
  const path = args[2];

  const fileStream = fs.createReadStream(path);

  const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity,
  });

  const key = `${prefix}_原始值`;

  rl.on("line", (line) => {
    if (line.length != 0) {
      console.log(JSON.stringify({ key: line }));
    }
  });

  await once(rl, "close");
}

main();
