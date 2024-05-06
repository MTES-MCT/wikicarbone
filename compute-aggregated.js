require("dotenv").config();
const fs = require("fs");
const { Elm } = require("./compute-aggregated-app");
const crypto = require("./lib/crypto");

const elmApp = Elm.ComputeAggregated.init({
  flags: {
    definitionsString: fs.readFileSync("public/data/impacts.json", "utf-8"),
    textileProcessesString: crypto.decryptFile("public/data/textile/processes_impacts.json"),
    foodProcessesString: fs.readFileSync("public/data/food/processes_impacts.json", "utf-8"),
  },
});

const exportJson = async (filepath, json) => {
  // Using dynamic import to avoid jest runtime error
  // eg. “A dynamic import callback was invoked without --experimental-vm-modules”
  fs.writeFileSync(filepath, JSON.stringify(json, null, 2));
};

elmApp.ports.export.subscribe(
  ({
    textileProcesses,
    foodProcesses,
    textileProcessesOnlyAggregated,
    foodProcessesOnlyAggregated,
  }) => {
    try {
      exportJson("public/data/textile/processes_impacts.json", textileProcesses);
      exportJson("public/data/food/processes_impacts.json", foodProcesses);
      exportJson("public/data/textile/processes.json", textileProcessesOnlyAggregated);
      exportJson("public/data/food/processes.json", foodProcessesOnlyAggregated);
      console.log("EXPORTED!");
    } catch (err) {
      console.error(err);
    }
  },
);

elmApp.ports.logError.subscribe((errorMessage) => {
  console.error("Error:", errorMessage);
});
