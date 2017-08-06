/* GLOBALS */
let punterID = -1;
let numPunters = -1;
let initialised = false;

let queuedClaims = [];
let queuedPass = false;

/* Graph rendering */

const colours =
  ["#1f77b4",
   "#aec7e8",
   "#ff7f0e",
   "#ffbb78",
   "#2ca02c",
   "#98df8a",
   "#d62728",
   "#ff9896",
   "#9467bd",
   "#c5b0d5",
   "#8c564b",
   "#c49c94",
   "#e377c2",
   "#f7b6d2",
   "#7f7f7f",
   "#c7c7c7",
   "#bcbd22",
   "#dbdb8d",
   "#17becf",
   "#9edae5"];

function getPunterColour(punter) {
  return colours[punter % colours.length];
}

var mapjson = {}

var maprdr = new FileReader;
maprdr.onload = function(ev) {
  mapjson = JSON.parse(ev.target.result);
  renderGraph(mapjson);
}

var moverdr = new FileReader;
moverdr.onload = function(ev) {
    var claims = JSON.parse(ev.target.result);
    var claimIndex = 0;

    var interval = null;
    var playback = function () {
        if (claimIndex >= claims.length) {
            clearInterval(interval);
            interval = null;
            return;
        }
        const claim = claims[claimIndex];
        updateEdgeOwner(claim.punter, claim.source, claim.target);
        claimIndex += 1;
    }
    interval = setInterval(playback, 200);
}
var movrdr = new Filereader;
movrdr.onload = function(ev) {
  
}

$("#mapjson").on("change", function(ev) {
  maprdr.readAsText(ev.target.files[0]);
});
$("#movejson").on("change", function(ev) {
  maprdr.readAsText(ev.target.files[0]);
});


function renderGraph(graph) {
    initCy(graph,
           function() {
               initialised = true;
               cy.autolock(true);
               bindCoreHandlers();
           }
          );
    return;
}

function setStatus(status) {
  $("#game-status").text(status);
}

function writeLog(msg) {
    let id = "log";
    let now = new Date(new Date().getTime()).toLocaleTimeString();
    document.getElementById(id).innerHTML += "(" + now + ") " + msg + "\n";
    document.getElementById(id).scrollTop = document.getElementById(id).scrollHeight;
    return;
}

function logInfo(msg) {
    writeLog("info: " + msg);
    return;
}

function logClaim(claim) {
    writeLog("move: punter #" + claim.punter + " claimed edge " +
             claim.source + " -- " + claim.target + ".");
    return;
}

function logPass(pass) {
    writeLog("pass: punter #" + pass.punter + ".");
    return;
}

function logScore(punter_id, score) {
  writeLog("punter #" + punter_id + " scored " + score);
}

function logMove(move) {
  if (move.claim != undefined) {
    logClaim(move.claim);
  } else if (move.pass != undefined) {
    logPass(move.pass);
  }
}

function logError(msg) {
    writeLog("error: " + msg);
    return;
}

function logRelay(msg) {
    writeLog("relay: " + msg);
    return;
}


/* EVENT HANDLING LOGIC */


function bindCoreHandlers() {
  cy.edges().on("mouseover", function(evt) {
    this.style("content", this.data("owner"));
  });
  cy.edges().on("mouseout", function(evt) {
    this.style("content", "");
  });
}

/* GAME UPDATE LOGIC */

function normaliseEdgeData(edgeData) {
  const src = edgeData.source;
  const trg = edgeData.target;
  if (trg < src) {
    let tmp = edgeData["source"];
    edgeData["source"] = edgeData["target"];
    edgeData["target"] = tmp;
  }
}

function updateEdgeOwner(punter, source, target) {
  const es = cy.edges("[source=\"" + source + "\"][target=\"" + target + "\"]");
  if (es.length > 0) {
    const e = es[0];
    e.data()["owner"] = punter;
    e.style("line-color", getPunterColour(punter));
  } else {
    logError("Trying to update nonexistent edge! (" + source + " -- " + target + ")");
  }
}

function printFinalScores(scores) {
  logInfo("Game finished!");
  for (let i = 0; i < scores.length; i++) {
    logScore(scores[i].punter, scores[i].score);
  }
}

function handleIncomingMoves(moves) {
  for (let i = 0; i < moves.length; i++) {
    handleIncomingMove(moves[i]);
  }

  if (initialised) {
    ourTurn();
  }
}

function handleIncomingMove(move) {
  logMove(move);
  if (move.claim !== undefined) {
    const claim = move.claim;
    normaliseEdgeData(claim);
    if (initialised) {
      updateEdgeOwner(claim.punter, claim.source, claim.target);
    } else {
      queueClaim(claim);
    }
  } else if (move.pass !== undefined) {
    if (!initialised) {
      queuedPass = true;
    }
  }
}

function queueClaim(claim) {
  queuedClaims.push(claim);
}

function playQueuedClaims() {
  for (let i = 0; i < queuedClaims.length; i++) {
    const claim = queuedClaims[i];
    updateEdgeOwner(claim.punter, claim.source, claim.target);
  }
  queuedClaims = [];
  queuedPass = false;
  ourTurn();
}
