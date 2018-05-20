const request = require('request');

var packages = [];
var packageVersions = [];
var modules = [];

const Elm = require('./parse');

function handleModules(artifact, version, cb) {
  const mod = modules.shift();
  if (!mod) {
    cb();
    return;
  }


  request(`https://raw.githubusercontent.com/${artifact.name}/beta-${version}/src/${mod.replace('.', '/')}.elm`, function(err, res, body) {
    if (res.statusCode != 200) {
      console.log(`> Could not load ${artifact.name}@${version} | ${mod}`);
      cb();
      return;
    }
    console.log(`${artifact.name}@${version} | ${mod}`);
    const before = new Date().getTime();
    Elm.Elm.Main.worker(body);
    const after = new Date().getTime();
    console.log(after - before);
    cb();
  })

}

function analyseVersion(artifact, version, definition, cb) {
  modules = definition['exposed-modules'];
  if (!Array.isArray(modules)) {
    cb();
    console.log(`${artifact.name}@${version} is a Core thingie`);
    return;
  }
  handleModules(artifact, version, cb)
}

function handleNextVersion(next, cb) {
  const nextVersion = packageVersions.shift();
  if (!nextVersion) {
    cb();
    return;
  }

  request(`https://github.com/${next.name}/raw/beta-${nextVersion}/elm.json`, function(err, res, body) {
    if (res.statusCode !== 200) {
      console.log(`> ${next.name}@${nextVersion} Could not fetch elm.json`);
      handleNextVersion(next, cb);
      return;
    }

    analyseVersion(next, nextVersion, JSON.parse(body), function() {
      handleNextVersion(next, cb);
    });
  });

}

function analysePackage(next, cb) {
  packageVersions = next.versions;

  handleNextVersion(next, function() {
    cb();
    return;
  })
}

function handleNextPackage() {
  const next = packages.shift();

  if (!next) {
    return;
  }

  analysePackage(next, function() {
    handleNextPackage();
  });
}


request('https://alpha.elm-lang.org/search.json', function(err, response, body) {
  packages = JSON.parse(body);

  handleNextPackage()
})