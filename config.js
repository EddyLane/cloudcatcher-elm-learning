System.config({
  baseURL: "/",
  defaultJSExtensions: true,
  transpiler: "typescript",
  paths: {
    "npm:*": "jspm_packages/npm/*",
    "*": "./src/ts",
    "github:*": "jspm_packages/github/*"
  },

  packages: {
    "./src/ts": {
      "defaultExtension": "ts"
    }
  },

  map: {
    "pouchdb": "github:pouchdb/pouchdb@5.1.0",
    "typescript": "npm:typescript@1.7.5"
  }
});
