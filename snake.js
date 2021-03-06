var Cell = (function () {
    function Cell(x, y) {
        this.x = x;
        this.y = y;
    }
    return Cell;
}());
var Snake = (function () {
    function Snake(head, tail) {
        this.head = head;
        this.tail = tail;
    }
    Snake.prototype.appendHead = function (cell) {
        return new Snake(cell, this);
    };
    Snake.prototype.length = function () {
        var s = this.tail;
        if (!s) {
            return 1;
        }
        var c = 2;
        while (s.tail) {
            c++;
            s = s.tail;
        }
        return c;
    };
    // check if cell intersects head
    Snake.prototype.xHead = function (cell) {
        if (this.head.x === cell.x &&
            this.head.y === cell.y) {
            return true;
        }
        return false;
    };
    // check if cell intersects any snake component
    Snake.prototype.xWhole = function (cell) {
        if (this.xHead(cell)) {
            return true;
        }
        var s = this.tail;
        while (s) {
            if (s.xHead(cell)) {
                return true;
            }
            s = s.tail;
        }
        return false;
    };
    return Snake;
}());
// snake advances w/o reaching the pellet
function advance(s) {
    var x = s.head.x;
    var y = s.head.y;
    switch (nextDir) {
        case keyLeft:
            x = x === 0 ? (numX - 1) : x - 1;
            break;
        case keyRight:
            x = (x + 1) % numX;
            break;
        case keyDown:
            y = (y + 1) % numY;
            break;
        case keyUp:
            y = y === 0 ? (numY - 1) : y - 1;
            break;
    }
    var outSnake = new Snake(new Cell(x, y), s);
    // if the snake hits the orb
    if (outSnake.head.x === orb.x && outSnake.head.y === orb.y) {
        orb = relocate(orb, outSnake);
    }
    else {
        outSnake = killEnd(outSnake);
    }
    return outSnake;
}
function relocate(orb, outSnake) {
    // lazy but effective: keep choosing a new random spot until
    // we find one that doesn't intersect the snake
    var xNew;
    var yNew;
    var outOrb;
    do {
        xNew = Math.floor(numX * Math.random());
        yNew = Math.floor(numY * Math.random());
        outOrb = new Cell(xNew, yNew);
    } while (outSnake.xWhole(outOrb));
    return outOrb;
}
function killEnd(s) {
    var orig = s;
    if (!s || !s.tail) {
        console.log("NULL WHAAA?");
        return null;
    }
    var par;
    while (s.tail) {
        par = s;
        s = s.tail;
    }
    par.tail = null;
    return orig;
}
var atomX = 18;
var atomY = 18;
var numX = 25;
var numY = 25;
var sand = "#f8c89f";
var snakeSkin = "#A6D785";
var orbColor = "#ffffff";
var canvas = InitCanvas();
var ctx = canvas.getContext("2d");
var loopMS = 80;
var orb = new Cell(14, 13);
function drawCell(cell, color) {
    ctx.fillStyle = color;
    ctx.strokeStyle = '#003300';
    ctx.lineWidth = 3;
    roundRect(ctx, atomX * cell.x, atomY * cell.y, atomX, atomY, (atomX + atomY) / 6, true, true);
}
function drawSnake(snake) {
    var color = snakeSkin;
    drawCell(snake.head, color);
    snake = snake.tail;
    while (snake) {
        drawCell(snake.head, color);
        snake = snake.tail;
    }
}
function drawOrb() {
    ctx.fillStyle = orbColor;
    ctx.beginPath();
    var centerX = orb.x * atomX + atomX / 2;
    var centerY = orb.y * atomY + atomY / 2;
    var radius = ((atomX + atomY) / 2) / 2;
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI, false);
    ctx.fillStyle = 'white';
    ctx.fill();
    ctx.lineWidth = 2.5;
    ctx.strokeStyle = '#703300';
    ctx.stroke();
}
function InitCanvas() {
    var canvas = document.createElement("canvas");
    canvas.width = atomX * numX;
    canvas.height = atomY * numY;
    canvas.style.backgroundColor = sand;
    return canvas;
}
var keyLeft = 37;
var keyUp = 38;
var keyRight = 39;
var keyDown = 40;
var nextDir = keyDown;
document.onkeydown = function (e) {
    nextDir = e.keyCode;
};
function roundRect(ctx, x, y, width, height, r, fill, stroke) {
    ctx.beginPath();
    ctx.moveTo(x + r, y);
    ctx.lineTo(x + width - r, y);
    ctx.quadraticCurveTo(x + width, y, x + width, y + r);
    ctx.lineTo(x + width, y + height - r);
    ctx.quadraticCurveTo(x + width, y + height, x + width - r, y + height);
    ctx.lineTo(x + r, y + height);
    ctx.quadraticCurveTo(x, y + height, x, y + height - r);
    ctx.lineTo(x, y + r);
    ctx.quadraticCurveTo(x, y, x + r, y);
    ctx.closePath();
    if (fill) {
        ctx.fill();
    }
    if (stroke) {
        ctx.stroke();
    }
}
function Main() {
    document.body.appendChild(canvas);
    var e = new Cell(8, 4);
    var d = new Cell(8, 5);
    var c = new Cell(8, 6);
    var b = new Cell(8, 7);
    var theSnake = new Snake(b, new Snake(c, new Snake(d, new Snake(e, null))));
    drawSnake(theSnake);
    setInterval(function () {
        console.log("In loop");
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        drawSnake(theSnake);
        drawOrb();
        theSnake = advance(theSnake);
        console.log("now we have:", theSnake);
    }, loopMS);
}
Main();
// Just some old-school, manual tests!
function RunTests() {
    // --- test length ---
    var s = new Snake(new Cell(3, 4), null);
    if (s.length() != 1) {
        console.log("Length 1 fail!");
        return;
    }
    s = s.appendHead(new Cell(4, 5));
    if (s.length() != 2) {
        console.log("Length 2 fail!");
        return;
    }
    // --- test head intersection ---
    if (!s.xHead(new Cell(4, 5))) {
        console.log("Failed to intersect cell!");
        return;
    }
    if (s.xHead(new Cell(8, 5))) {
        console.log("False head intersection!");
        return;
    }
    // --- test whole intersection ---
    s = s.appendHead(new Cell(8, 12));
    if (!s.xWhole(new Cell(8, 12))) {
        console.log("First whole fail");
        return;
    }
    if (!s.xWhole(new Cell(4, 5))) {
        console.log("Second whole fail");
        return;
    }
    if (!s.xWhole(new Cell(3, 4))) {
        console.log("Second whole fail");
        return;
    }
    if (s.xWhole(new Cell(9, 9))) {
        console.log("False positive whole intersection");
        return;
    }
    // --- test removing from Snake ---
    var testSnake = new Snake(new Cell(3, 4), new Snake(new Cell(3, 5), null));
    testSnake = killEnd(testSnake);
    if (testSnake.head.x != 3 || testSnake.head.y != 4) {
        console.log("corrupted head");
        return;
    }
    if (testSnake.tail != null) {
        console.log("corrupted tail");
        return;
    }
    console.log("Woohoo, all the tests pass!");
}
//RunTests();
