function evtJson(evt, f) {
    var str;
    if(evt.data instanceof Blob) {
        var reader = new FileReader();
        reader.onload = function() {
            f($.parseJSON(reader.result));
        }
        reader.readAsText(evt.data);
    } else {
        f($.parseJSON(evt.data));
    }
}

// a map that first converts the keys to string
function SerMap() {
  this.map = new Map();
}
SerMap.prototype.set = function(k,v) {
  this.map.set(""+k,v);
}
SerMap.prototype.get = function(k) {
  return this.map.get(""+k);
}

// specialized atom map, can cause problems with high (> 2000) client ids
function AtomMap() {
  this.map = {};
}
AtomMap.prototype.set = function(k,v) {
  this.map[k[0]*k[1]+1] = v;
}
AtomMap.prototype.get = function(k,v) {
  return this.map[k[0]*k[1]+1];
}

spliceArr = function(arr,i,n,a) {
  a.unshift(i,n);
  arr.splice.apply(arr,a);
  a.shift();
  a.shift();
  return arr;
}

concatArr = function(arr1,arr2) {
    var a = arr1.slice(0);
    a.push.apply(a,arr2);
    return a;
}

var clientId = -1;
// we work with server-synchronized strictly increasing timestamps
var tsOffset = 0;
var prevTs   = 0;

function setClientId(cid) {
//  console.log("setting clientid");
//  console.log(cid);
  clientId = cid;
}

function updateTimestamp(t) {
    tsOffset = t - new Date().getTime();
}

function getTimestamp() {
    var t = new Date().getTime() + tsOffset;
    if(t <= prevTs) t = prevTs + 1;
    prevTs = t;
    return t;
}

function mkAtomId() {
    return [getTimestamp(),clientId];
}

function Document() {
   this.lines = [[0,0]];          // lines[line][ch] -> [time,clientid], first ch of every line is line separator atomid (or [0,0] for first)
   this.atoms = new AtomMap();    // [time,clientid] -> { id, prev, next, ch, removed }
   var firstId = [0,0];
   this.atoms.set(firstId, { id: firstId, t: 0, c: 0, prev: null, next: null, ch: ' ', removed: true });
}


// first atom
Document.prototype.first = function() {
   return this.atoms.get([0,0]);
}

// set document to list of [clientid,time,char,removed]
// expects first atom id to be [0,0]
Document.prototype.setDocument = function(d) {
//  console.log('setdocument');
//  console.log(d);
  this.lines = [];
  this.atoms = new AtomMap();
  var currentLine = [];
  var atomId = function(n) { return [d[n][0],d[n][1]] };
  var newAtoms = new Array(d.length);
  for(var i=0;i<d.length;i++) {
    var x = d[i];
    var aid    = atomId(i);
    var atom   = { id: aid, ch: x[2], removed: x[3] };
    newAtoms[i] = atom;
    this.atoms.set(aid,atom);
    if(!atom.removed || (aid[0] == 0 && aid[1] == 0)) {
      if(atom.ch == '\n') {
        this.lines.push(currentLine);
        currentLine = [];
      }
      currentLine.push(aid);
    }
  }
  this.lines.push(currentLine);
  for(var i=0;i<newAtoms.length;i++) {
    newAtoms[i].prev = (i > 0)                   ? newAtoms[i-1] : null;
    newAtoms[i].next = (i < newAtoms.length - 1) ? newAtoms[i+1] : null;
  }
}

// returns current version as string
Document.prototype.currentVersion = function() {
  var str = '';
  var a = this.first();
  while(true) {
    // console.log('iteration');
    // console.log(a);
    if(!a.removed) {
      str += a.ch;
    }
    if(!a.next) break;
    a = a.next;
  }
  return str;
}

function aidEq(aid1,aid2) {
  if(aid1 == null && aid2 == null) return true;
  if(aid1 == null || aid2 == null) return false;
  return (aid1[0] == aid2[0] && aid1[1] == aid2[1]);
}

function posEq(pos1,pos2) {
   return pos1.ch == pos2.ch && pos1.line == pos2.line;
}

// replace a range { line, ch } - { line, ch } by str
// returns modification messages
Document.prototype.replace = function(from,to,strs) {
  var mods  = [];
  var start = this.idbefore(from); // insert new string after this atom
  if(start == null) start = [0,0];
  var end   = this.pos2atomid(to);                // insert new string before this
  var aid   = this.nextid(start);
  var beforeEnd = this.idbefore(to); // last char to be removed
//  console.log("start: " + this.atomStr(start) + " end: " + this.atomStr(end));
//  console.log("aid: " + aid + " beforeEnd: " + beforeEnd);
  // first mark atoms as removed
  var atom = (aid != null) ? this.atoms.get(aid) : null;
  while(atom != null && !aidEq(atom.id,end) && !posEq(from,to)) {
    if(!atom.removed) {
      atom.removed = true;
      mods.push({ action: 'remove', atom: atom.id });
    }
    if(aidEq(atom.id, beforeEnd)) {
      break;
    }
    atom = atom.next;
  }
  // now remove original string
  var lastPost = this.lines[to.line].slice(to.ch+1); // part that we have to stick back to the last line
  var firstPre = this.lines[from.line].slice(0,from.ch+1);
  this.lines.splice(from.line, to.line - from.line + 1); // fixme don't do this if single line
  // insert new chars
  var newAtoms = [];
  var newLines = [];
  var currentLine = [];
  for(var l=0;l<strs.length;l++) {
    var str = strs[l];
    for(var i=0;i<str.length;i++) {
      var aid = mkAtomId();
      var atom = { id: aid, ch: str[i], removed: false }
      newAtoms.push(atom);
      currentLine.push(aid);
    }
    newLines.push(currentLine);
    currentLine = [];
    if(l<strs.length-1) {
      var linebr = { id: mkAtomId(), ch: '\n', removed: false};
      newAtoms.push(linebr);
      currentLine.push(linebr.id);
    }
  }
  for(var i=0;i<newAtoms.length;i++) {
      if(i>=1) { newAtoms[i].prev = newAtoms[i-1]; }
      if(i<newAtoms.length-1) { newAtoms[i].next = newAtoms[i+1]; }
  }
  if(newAtoms.length > 0) {
      var startAtom  = this.atoms.get(start);
      var afterStart = this.atoms.get(start).next;
      newAtoms[0].prev = startAtom;
      startAtom.next = newAtoms[0];
      newAtoms[newAtoms.length-1].next = afterStart;
      if(afterStart != null) { afterStart.prev = newAtoms[newAtoms.length-1]; }
  }
  for(var i=0;i<newAtoms.length;i++) {
      var a = newAtoms[i];
      mods.push({ action: 'insert', after: a.prev.id, id: a.id,  ch: a.ch });
      this.atoms.set(a.id,a);
  }
  if(newLines.length > 0) {
    newLines[0] = concatArr(firstPre,newLines[0]);
    newLines[newLines.length-1] = concatArr(newLines[newLines.length-1],lastPost);
    spliceArr(this.lines,from.line, 0, newLines);
  } else {
    this.lines.splice(from.line,0,concatArr(firstPre,lastPost));
  }
  this.dump();
  return mods;
}

// apply an insert/delete operation
Document.prototype.applyOp = function(op) {
  function smaller(aid1, aid2) {
    return aid1[0] < aid2[0] || (aid1[0] == aid2[0] && aid1[1] < aid2[1]);
  }
  function isFirst(aid) {
    return aid[0] == 0 && aid[1] == 0;
  }

  if(op.action == 'insert') {
//    console.log("inserting");
//    console.log(op);
    var atomAfter = this.atoms.get(op.after);
    if(this.atoms.get(op.id) != null) {
	return false;  // already applied op
    } else if(atomAfter == null) {
      console.log("invalid position");
      console.log(atomAfter);
      console.log(op.id);
      console.log(this.dumpStructure());
      return false;
    } else {
      // first find insertion position
      var atom = atomAfter;
      while(true) {
        if(atom.next == null || smaller(atom.next.id, op.id)) {
          var newAtom = { id: op.id, ch: op.ch, removed: false, prev: atom, next: atom.next }
          if(atom.next != null) {
            atom.next.prev = newAtom;
          }
          atom.next = newAtom;
          this.atoms.set(newAtom.id, newAtom);
          break;
        }
        atom = atom.next;
      }
  //    console.log("inserted after:");
  //    this.dumpAtom(aid);
      // now search back to first visible, or [0,0]
      // aid = op.after;
      while(!isFirst(atom.id) && atom.removed) {
         atom = atom.prev;
      }
      // find aid in lines
      idx = this.findIndex(atom.id);
      var l=idx[0];
      var c=idx[1];

//      console.log(this.dumpStructure());
//      console.log("line index: " + l + ":" + c);
      // and add our new id
      if(op.ch == '\n') {
        var remain = this.lines[l].slice(c+1);
        this.lines[l] = this.lines[l].slice(0,c+1);
        remain.unshift(op.id);
        this.lines.splice(l+1,0,remain);
      } else {
        this.lines[l].splice(c+1,0,op.id);
      }
//      console.log(this.dumpStructure());
      return true;
    }
  } else if(op.action == 'remove') {
    var atom = this.atoms.get(op.id);
    if(atom == null || atom.removed) {
      return false;
    } else {
      atom.removed = true;
      idx = this.findIndex(atom.id);
      var l = idx[0];
      var c = idx[1];
      if(atom.ch == '\n') {
        var remain = this.lines[l].slice(1);
        this.lines[l-1] = concatArr(this.lines[l-1], remain);
        this.lines.splice(l,1);
      } else {
        this.lines[l].splice(c,1);
      }
      return true;
    }
  } else {
    return false;
  }
}

Document.prototype.findIndex = function(aid) {
  for(var l=0;l<this.lines.length;l++) {
    var cl = this.lines[l];
    for(var c=0;c<cl.length;c++) {
      if(cl[c][0] == aid[0] && cl[c][1] == aid[1]) {
        return [l,c];
      }
    }
  }
  return null;
}

// convert a line,ch position to an atom id
Document.prototype.pos2atomid = function(pos) {
  return this.lines[pos.line][pos.ch+1];
}

// atom id before cursor { line: n, ch: m }  (both start at 0)
Document.prototype.idbefore = function(pos) {
  return this.lines[pos.line][pos.ch];
}

Document.prototype.dumpAtom = function(aid) {
  console.log(this.atomStr(aid)); 
}

Document.prototype.dumpLine = function(aids) {
  console.log(this.lineStr(aids));
}

Document.prototype.lineStr = function(aids) {
  var str='';
  for(var i=0;i<aids.length;i++) {
    str += this.atomStr(aids[i]);
  }
  return str;
}

Document.prototype.atomStr = function(aid) {
  if(aid == null) {
      return "[null]";
  }
  var atom = this.atoms.get(aid);
  var str  = '';
  function charify(c) { return (c=='\n') ? "\\n" : c; }
  if(atom == null) {
    str += "[" + [aid[0],aid[1]] + "](not found)";
    aid = null;
  } else {
    str += "[" + [aid[0],aid[1],"'" + charify(atom.ch) + "'",atom.removed] + "]";
    aid = atom.next;
  }
  return str;
}

// fixme remove usage of this
Document.prototype.previd = function(aid) {
  var atom = this.atoms.get(aid);
  if(atom == null || atom.prev == null) return null;
  return atom.prev.id;
}

// fixme remove usage of this
Document.prototype.nextid = function(aid) {
  var atom = this.atoms.get(aid);
  if(atom == null || atom.next == null) return null;
  return atom.next.id;
}

Document.prototype.dump = function() {
  console.log(this.dumpStructure());
}

Document.prototype.dumpStructure = function() {
  return ''; // comment out for dumps
  str = "map:\n";
  var aid = [0,0]; // this.first();
  while(aid != null) {
    str += this.atomStr(aid) + "\n";
    aid = this.nextid(aid);
  }
  str += "lines:\n";
  for(var i=0;i<this.lines.length;i++) {
    str += this.lineStr(this.lines[i]) + "\n";
  }
  str += "current:\n" + this.currentVersion();
  return str;
}

