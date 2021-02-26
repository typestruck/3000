var addon = require('../native');

for (let i = 0; i < 30; ++i) {
    console.log(addon.generate(0, 100));
    console.log("\n");
}


