require("dotenv").config();
const fs = require("fs");
const express = require("express");
const bodyParser = require("body-parser");
const cors = require("cors");
const yaml = require("js-yaml");
const helmet = require("helmet");
const Sentry = require("@sentry/node");
const { Elm } = require("./server-app");
const lib = require("./lib");

const app = express(); // web app
const api = express(); // api app
const host = "0.0.0.0";
const port = 8001;

// Env vars
const { SENTRY_DSN, MATOMO_HOST, MATOMO_SITE_ID, MATOMO_TOKEN } = process.env;

// Matomo
if (process.env.NODE_ENV !== "test" && (!MATOMO_HOST || !MATOMO_SITE_ID || !MATOMO_TOKEN)) {
  console.error("Matomo environment variables are missing. Please check the README.");
  process.exit(1);
}

// Sentry
if (SENTRY_DSN) {
  Sentry.init({ dsn: SENTRY_DSN, tracesSampleRate: 0.1 });
  // Note: Sentry middleware *must* be the very first applied to be effective
  app.use(Sentry.Handlers.requestHandler());
}

// Web

// Note: helmet middlewares have to be called *after* the Sentry middleware
// but *before* other middlewares to be applied effectively
app.use(
  helmet({
    crossOriginEmbedderPolicy: false,
    hsts: false,
    xssFilter: false,
    contentSecurityPolicy: {
      useDefaults: true,
      directives: {
        "default-src": [
          "'self'",
          "https://api.github.com",
          "https://raw.githubusercontent.com",
          "https://sentry.incubateur.net",
          "*.gouv.fr",
        ],
        "frame-src": ["'self'", `https://${process.env.MATOMO_HOST}`, "https://www.loom.com"],
        "img-src": [
          "'self'",
          "data:",
          "blob:",
          "https://avatars.githubusercontent.com/",
          "https://raw.githubusercontent.com",
        ],
        // FIXME: We should be able to remove 'unsafe-inline' as soon as the Matomo
        // server sends the appropriate `Access-Control-Allow-Origin` header
        // @see https://matomo.org/faq/how-to/faq_18694/
        "script-src": ["'self'", "'unsafe-inline'", `https://${process.env.MATOMO_HOST}`],
        "object-src": ["blob:"],
      },
    },
  }),
);

app.use(
  express.static("dist", {
    setHeaders: (res) => {
      // Note: helmet sets this header to `0` by default and doesn't allow overriding
      // this value
      res.set("X-XSS-Protection", "1; mode=block");
    },
  }),
);

// Redirects: Web
app.get("/accessibilite", (_, res) => res.redirect("/#/pages/accessibilité"));
app.get("/mentions-legales", (_, res) => res.redirect("/#/pages/mentions-légales"));
app.get("/stats", (_, res) => res.redirect("/#/stats"));

// API

const openApiContents = yaml.load(fs.readFileSync("openapi.yaml"));

// Matomo
const apiTracker = lib.setupTracker(openApiContents);

const elmApp = Elm.Server.init();

elmApp.ports.output.subscribe(({ status, body, jsResponseHandler }) => {
  return jsResponseHandler({ status, body });
});

api.get("/", (req, res) => {
  apiTracker.track(200, req);
  res.status(200).send(openApiContents);
});

// Redirects: API
api.get(/^\/countries$/, (_, res) => res.redirect("textile/countries"));
api.get(/^\/materials$/, (_, res) => res.redirect("textile/materials"));
api.get(/^\/products$/, (_, res) => res.redirect("textile/products"));
const cleanRedirect = (url) => (url.startsWith("/") ? url : "");
api.get(/^\/simulator(.*)$/, ({ url }, res) => res.redirect(`/api/textile${cleanRedirect(url)}`));

// Note: Text/JSON request body parser (JSON is decoded in Elm)
api.all(/(.*)/, bodyParser.json(), async (req, res) => {
  let processesFilename = "processes.json";
  if (req.headers.token) {
    // TODO: there is no proxy in dev, so it needs to be the django's url
    // const checkTokenUrl = `http://127.0.0.1:8000/accounts/check_token/`;
    const checkTokenUrl = `http://${host}:${port}/accounts/check_token/`;
    const isTokenValidRes = await fetch(
      checkTokenUrl,
      { headers: { "token": req.headers.token }});
    const isTokenValid = isTokenValidRes.status == 200;
    if (isTokenValid) {
      // The request is authentified.
      processesFilename = "processes_impacts.json";
    } else {
      return res.status(401).send(JSON.stringify({"error": "Token is invalid"}));
    }
  }
  let processes;
  try {
    const foodRes = await fetch(`http://${host}:${port}/data/food/${processesFilename}`);
    const foodProcesses = await foodRes.json();
    const textileRes = await fetch(`http://${host}:${port}/data/textile/${processesFilename}`);
    const textileProcesses = await textileRes.json();
    processes = {"foodProcesses": JSON.stringify(foodProcesses), "textileProcesses": JSON.stringify(textileProcesses)};
  } catch (err) {
    console.error(err.message);
  }
  
  elmApp.ports.input.send({
    method: req.method,
    url: req.url,
    body: req.body,
    processes: processes,
    jsResponseHandler: ({ status, body }) => {
      apiTracker.track(status, req);
      res.status(status).send(body);
    },
  });
});

api.use(cors()); // Enable CORS for all API requests
app.use("/api", api);

// Sentry error handler
// Note: *must* be called *before* any other error handler
if (SENTRY_DSN) {
  app.use(Sentry.Handlers.errorHandler());
}

const server = app.listen(port, host, () => {
  console.log(`Server listening at http://${host}:${port}`);
});

module.exports = server;
