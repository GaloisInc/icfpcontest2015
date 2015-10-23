// 

var radius  = 10
var toSide  = radius * Math.sin(Math.PI / 3)
var spacing = radius / 5


function setRadius(r) {
  radius  = r
  toSide  = radius * Math.sin(Math.PI / 3)
  spacing = radius / 5
}


function gridToScreen(x,y) {
  var ch = (y % 2 === 0) ? 0 : toSide
  return { x: ch + x * (2 * toSide + spacing), y: y * (1.5 * radius + spacing) }
}

function drawSpot(cl,x,y,here) {
  var loc = gridToScreen(x,y)
  var w = 2 * toSide
  var h = 2 * radius
  var d = $('<div/>')
         .addClass('spot').addClass(cl)
         .css('left', loc.x)
         .css('top', loc.y)
         .css('width', w)
         .css('height', h)
         .css('line-height', h + 'px')
  here.append(d)
  return d
}

function drawCenter(b,here) {
  var x = b.pivot.x
  var y = b.pivot.y
  var scale = 3
  var hw = 2 * toSide
  var hh = 2 * radius
  var loc = gridToScreen(x,y)
  var w = hw / scale
  var h = hh / scale

  here.append($('<div/>')
              .addClass('center')
              .css('left', loc.x + (hw - w) / 2)
              .css('top', loc.y + (hh - h) / 2)
              .css('width', w)
              .css('height', h) )
}

function drawPiece(xs, here) {
  jQuery.each(xs.members, function(ix, p) {
    drawSpot('piece', p.x, p.y,here)
  })
  drawCenter(xs,here)
}

function drawShapes(bs, here) {

  function dim(spots) {
    var maxX = 0, maxY = 0
    jQuery.each(spots, function(ix,s) {
      if (s.x > maxX) maxX = s.x
      if (s.y > maxY) maxY = s.y
    })
    return { width: maxX + 1, height: maxY + 1 }
  }

  jQuery.each(bs, function(ix,b) {
    var ds = dim(b.members)
    var d = drawContainer(ds.width,ds.height).addClass('shape')
    jQuery.each(b.members, function(ix,p) {
      drawSpot('full', p.x, p.y, d)
    })
    drawCenter(b,d)
    here.append(d)
  })

}

function drawContainer(w,h) {
  return $('<div/>')
          .addClass('board')
          .css('width',  (w + 0.5) * (spacing + 2 * toSide) - spacing)
          .css('height', h * (1.5 * radius + spacing) + spacing)
}

function drawBoard(b, here, spotFun) {
  var d = drawContainer(b.width,b.height)

  jQuery.each(b.layout, function(y, row) {
    jQuery.each(row, function(x, full) {
      var s = drawSpot(full ? 'full' : 'blank', x, y, d)
      if (spotFun !== undefined) spotFun(s)
    })
  })
  here.append(d)
  return d
}

function drawStep(s, here) {
  var d = drawBoard(s.board, here)
  drawPiece(s.piece, d)
}

function server(here, x,p) {
  jQuery.post('http://localhost:8000/' + x, p
             , function(g) { here.empty(); drawGame(g,here) })
}

function drawHelp(here) {
  var t = $('<table/>').addClass('help')
  function row(x,y) { t.append($('<tr/>')
                               .append($('<td/>').addClass('key').text(x))
                               .append($('<td/>').text(y))) }
  row('h', 'move left')
  row('l', 'move right')
  row('j', 'move down left')
  row('k', 'move down right')
  row('u', 'rotate anti-clockwise')
  row('i', 'rotate clockwise')
  row('s', 'start over')
  here.append(t)
}

function addKeys(here) {
  here.keydown(function(ev) {
    switch(ev.which) {
      case /*h*/ 72 : return server(here, 'left', {})
      case /*j*/ 74 : return server(here, 'down_left', {})
      case /*k*/ 75 : return server(here, 'down_right', {})
      case /*l*/ 76 : return server(here, 'right', {})
      case /*u*/ 85 : return server(here, 'rot_anti', {})
      case /*i*/ 73 : return server(here, 'rot_clock', {})
      case /*s*/ 83 : return server(here, 'start', {})
      default: console.log(ev.which)
    }
  })
}

function replay(here,delay,moves) {

  function doMove(i) {
    if (i >= moves.length) return;

    var move = moves.charAt(i)
    switch(move) {
      case 'p':
      case '\'':
      case '.':
      case '0':
      case '3':
      case '!':
                server(here, 'left', {})
                break

      case 'b':
      case 'c':
      case 'e':
      case 'f':
      case 'y':
      case '2':
                server(here, 'right', {})
                break
      case 'a':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case '4':
                server(here, 'down_left', {})
                break

      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case ' ':
      case '5':
                server(here, 'down_right', {})
                break

      case 'd':
      case 'q':
      case 'r':
      case 'v':
      case 'z':
      case '1':
                server(here, 'rot_clock', {})
                break

      case 'k':
      case 's':
      case 't':
      case 'u':
      case 'w':
      case 'x':
                server(here, 'rot_clock', {})
                break

      case '~': server(here, 'start', {})
                break

      default: console.log(move)
    }

    setTimeout(function() { doMove(i+1) }, delay)
  }

  doMove(0)
}


function drawPower(x,here,k) {
  var d = $('<div/>')
          .text(x)
          .css('font-size', '150px')
          .css('position', 'absolute')
          .css('left', '1em')
          .css('top', '1em')
          .css('font-family', "'Nosifer', cursive")
          .hide()
  here.append(d)
  d.effect('slide', { complete: function() {
    //here.effect('shake', { duration: 'slow', complete: function() {
    d.effect('explode', { complete: function() {
    d.remove()
    k()

  }})}})//}})

}

// For making movies
function drawFrames(delay, fs, here) {
  var frame = 0;

  function go() {
    if (frame >= fs.length) return

    switch(fs[frame].tag) {
      case 'step':
        here.empty()
        drawStep(fs[frame].step, here)
        frame += 10
        setTimeout(go, delay)
        break;
      case 'power':
        // drawPower(fs[frame++].power,here,go)
        ++frame;
        setTimeout(go,0)
    }
  }

  go()
}


function drawScore(g,here) {

  here.append($('<div/>').text('Score: ' + g.score +
                               ' previous lines: ' + g.prevLines +
                               ' locked: ' + g.locked +
                               ' moves: ' + g.moves))
}

function drawGame(g, here) {
  var s = g.state

  switch(s.tag) {

    case 'play':
      drawStep(s.step,here)
      break

    case 'error':
      drawStep(s.step,here)
      here.append($('<div/>').text('Error!'))
      break

  case 'full':
      drawBoard(s.board,here)
      here.append($('<div/>').text('The board is full'))
      break

  case 'end':
      drawBoard(s.board,here)
      here.append($('<div/>').text('We are out of shapes'))
      break
  }

  drawScore(g, here)
  drawShapes(g.pieces, here)
  // drawHelp(here)
}


