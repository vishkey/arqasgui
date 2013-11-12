//Creamos una especializacion de TextInput para introducir vectores numéricos
var vectorInputBinding = new Shiny.InputBinding();
$.extend(vectorInputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-vector-input');
    },
	getId: function(el) {
      return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
    },
	getValue: function(el) {
	  res = new Array();
	  numbers = el.value.split(";")
	  for(i=0; i<numbers.length; i++)
		res.push(parseInt(numbers[i]));
      return (res);
    },
	subscribe: function(el, callback) {
      $(el).on('change.vectorInputBinding', function(event) {
        callback(false);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.vectorInputBinding');
    },
	receiveMessage: function(el, data) {
		  if (data.hasOwnProperty('value')) {
			res = "";
			for(v in data.matrix)
				res += v + "; "
			this.setValue(el, res.substr(0, res.length-2));
		 }
		 
		  if (data.hasOwnProperty('label'))
			$(el).parent().find('label[for=' + el.id + ']').text(data.label);

		  $(el).trigger('change');
		},
	getState: function(el) {
      return {
        label: $(el).parent().find('label[for=' + el.id + ']').text(),
        value: getValue(el)
      };
    },
	initialize: function(el) {
	}
});
Shiny.inputBindings.register(vectorInputBinding, 'shiny.vectorInputBinding');


//Creamos un InputBinding para introducir matrices
var matrixInputBinding = new Shiny.InputBinding();
$.extend(matrixInputBinding, {
	find: function(scope) {
		return $(scope).find('.shiny-matrix-input');
	},
	getId: function(el) {
		return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
	},
	getValue: function(el) {
		return {
			matrix: this.matrix[$(el).attr("id")]
			}
	},
	setValue: function(el, value) {
	},
	subscribe: function(el, callback) {
		var id = $(el).attr("id");
		var self = this;
		$("#" + id + "-numNodes").on("change", function(event) {
			event.stopImmediatePropagation();
			var prevn = parseInt($(this).attr("prev-value"));
			var n = $(this).val();
			if (n < 11 && n > 0) {
				if (prevn < n) {
					self._expandTable(id, prevn,  n-prevn);
					var neww = $("#" + id + "-manualDialog").dialog("option", "width") + 80*(n-prevn);
					var newh = $("#" + id + "-manualDialog").dialog("option", "height") + 25*(n-prevn)
					if (neww > 1200) neww = 1200;
					if (newh > 520) newh = 520;
					
					$("#" + id + "-manualDialog").dialog("option", "width", neww);
					$("#" + id + "-manualDialog").dialog("option", "height", newh);
				} else {
					self._reduceTable(id, prevn, prevn-n);
					var neww = $("#" + id + "-manualDialog").dialog("option", "width") - 80*(prevn-n);
					var newh = $("#" + id + "-manualDialog").dialog("option", "height") - 25*(prevn-n);
					
					$("#" + id + "-manualDialog").dialog("option", "width", neww);
					$("#" + id + "-manualDialog").dialog("option", "height", newh);
				}
				$(this).attr("prev-value", $(this).val()); 	
			}
			$(this).val($(this).attr("prev-value"));
		});
		
		$(el).on("send.matrixInputBinding", function(event) {
			callback(false);
		});
	},
	initialize: function(el) {	
		if(!this.hasOwnProperty("matrix"))
			this.matrix = new Array();	
			
		var id = $(el).attr("id");		
		$(el).append("<button id='"+ id +"-manualInput'>Manual Input</button>" +
					  "<div id='"+ id +"-manualDialog' style='maxHeight:520; maxWidth:1200'>" +
							"<label for='" + id + "-numNodes'>Number of nodes: </label><input id='"+ id +"-numNodes' type=number value=5 prev-value=5 min=1 max=10/><br><br>" +
							"<center><table id='" + id + "-table'></table></center>" +
					  "</div>");
		 var nodes;
		if ($(el).attr("ini-value") != null) {
			inivalue = $(el).attr("ini-value").split("],[");
			nodes = inivalue.length;
			this.matrix[id] = new Array(nodes);
			for(i=0; i<inivalue.length; i++) {
				rowvalues = inivalue[i].replace("[", "").replace("]","").split(",");
				this.matrix[id][i] = new Array(nodes);
				for(j=0; j<rowvalues.length; j++)
					this.matrix[id][i][j] = parseFloat(rowvalues[j]);
			}
			$("#" + id + "-numNodes").val(nodes).attr("prev-value", nodes);
		}
		else{
			nodes = parseInt($("#" + id + "-numNodes").val());
			if (!this.matrix.hasOwnProperty(id)) {
				this.matrix[id] = new Array(nodes);
				for (var i=0; i<nodes; i++) {
					this.matrix[id][i] = new Array(nodes);
					for (var j=0; j<nodes; j++) {
						this.matrix[id][i][j] = 0;
					}
				}	
			}
		}
		console.log(this.matrix[id].toString());
		this._expandTable(id, 0 , nodes);
		var self = this;
		$("#" + id + "-manualDialog").dialog({
		    autoOpen: false,
			resizable: false,
			height: 400,
			width: 800,
			resizable: true,
			modal: true,
			buttons: {
				"Reset Inputs": function() {
					$("input[id$='-input']").each(function(i, v) {
						$(v).val(0);
						idSep = $(v).attr("id").split('-');
						i = idSep[2]-1;
						j= idSep[3]-1;
						self.matrix[id][i][j] = 0;
					});
					$("input[id$='-sum']").each(function(i, v) {
						$(v).val(1);
					});
				},
				Ok: function() {
					$(el).trigger("send.matrixInputBinding");
					$(this).dialog("close");
				},
				Cancel: function() {
					$(this).dialog("close");
				}
			}
		});
		
		$("#" + id + "-manualInput").button().click(function() {
			$("#" + id + "-manualDialog").dialog("open");
		});
	},
	_expandTable: function(id, prevn, n) {
		//Expandimos la cabecera
		var header = rows = "";
		var self = this;
		for (var i=Math.max(1, prevn+1); i<=prevn+n; i++)
			header += "<th width=50>Node " + i + "</th>";
		if (prevn == 0) {
			header = "<tr id='" + id + "-table-0'><th>From\\To</th>" + header;
			header += "<th width=50>Exit</th></tr>"
			$("#" + id + "-table").append(header);
		} else {
			$("#" + id + "-table th:last").before(header);
		}
		
		//Anadimos en las filas anteriores los nuevos inputs si no habia tabla nos saltamos este paso
		if (prevn != 0) {
			for (var i=1; i<=prevn; i++)  {
				var tds = "";
				self.matrix[id][i-1].length += n;
				//$("#" + id + "-table-" + i + "-sum").val(self.matrix[id][i-1][prevn];)
				for (var j=prevn+1; j <= prevn+n; j++) {
					tds += "<td><input id='" + id + "-table-" + i + "-" + j + "-input' type='number' style='width:6em' step=0.05 value=0 max=1 min=0/></td>";
					self.matrix[id][i-1][j-1] = 0;
				}
				$("#" + id + "-table-" + i + " td:last").before(tds);
			}
			self.matrix[id].length += n;
		}	
		//Anadimos las nuevas filas
		for (var i=prevn+1; i<=prevn+n; i++) {
			rows += "<tr id='" + id + "-table-" + i + "'><td>Node " + i + "</td>";
			if (prevn != 0) 
				self.matrix[id][i-1] = new Array(prevn+n);
			var tot = 0;
			for (var j=1; j <=prevn+n; j++) {
				if(prevn != 0) 
					self.matrix[id][i-1][j-1] = 0;
				rows += "<td><input id='" + id + "-table-" + i + "-" + j + "-input' type='number' style='width:6em' step=0.05 value=" + self.matrix[id][i-1][j-1] +" max=1 min=0/></td>";
				tot += self.matrix[id][i-1][j-1];
			}
			rows += "<td><input id='"+ id + "-table-" + i + "-sum' type='number' style='width:6em' value=" + (1-tot) +" disabled /></td></tr>";
		}
		$("#" + id + "-table").append(rows);
		
		$("input[id$='-input']").each(function(i, v) {
			$(v).on("change", function(event) {
				tot = 0;
				idRow = $(event.target).parent().parent().attr("id");
				$("input[id^='" + idRow + "'][id$='-input']").each(function(i, v) {
					tot += parseFloat($(v).val());
				});
				idSep = $(event.target).attr("id").split('-');
				i = idSep[2]-1;
				j = idSep[3]-1;
				self.matrix[id][i][j] = parseFloat($(v).val());
				//self.matrix[id][i][prevn+n] = 1-tot;
				$("#" + idRow + "-sum").val(1-tot);
			});
		});
	},
	_reduceTable: function(id, prevn, n) {
		//Eliminamos las últimas filas
		for(var i=prevn-n+1; i<=prevn; i++) {
		-	$("#" + id + "-table-" + i).remove();
		}
		this.matrix[id].length -= n;
		
		//Eliminamos las celdas de las n penúltimas columnas en todas las filas restantes
		for(var i=1; i<=prevn-n; i++) {
			var probLost = 0;
			for (var j=(prevn-n); j<prevn; j++) {
				$("#" + id + "-table-" + i + " td:nth-last-child(2)").remove();
				probLost += this.matrix[id][i-1][j];
			}	
			prevval = parseFloat($("#" + id + "-table-" + i + "-sum").val());
			$("#" + id + "-table-" + i + "-sum").val(prevval+probLost);
			this.matrix[id][i-1].length -= n;
		}
		
		//Eliminamos la cabeceras de las columnas eliminadas
		for(var j=1; j<=n; j++)
			$("#" + id + "-table-0 th:nth-last-child(2)").remove();
		
	}
});
Shiny.inputBindings.register(matrixInputBinding, 'shiny.matrixInputBinding');

var networkOutputBinding = new Shiny.OutputBinding();
$.extend(networkOutputBinding, {
	find: function(scope) {
	  return $(scope).find('.shiny-network-output');
    },
    renderValue: function(el, data) {
		var id = $(el).attr("id");
		if(!this.hasOwnProperty("particleSystem")) {
			console.log("No tiene particleSystem");
			this.particleSystem = new Array();
		}
		if (data != null) {
			$(el).empty();
			$(el).append("<canvas id='" + id + "-graphcanvas' width=850 height=540></canvas>");
			this.particleSystem[id] = arbor.ParticleSystem(1000, 50, 0.5, true);
			this.particleSystem[id].renderer = new this._Renderer("#" + id + "-graphcanvas");
			console.log("Renderizando " + id + ": " + data.s.length);	
			for(i=0;  i<data.s.length; i++) {
				console.log("s: ", data.s[i]);
				this.particleSystem[id].addNode(i+1, {s: data.s[i]});
			}
			for(i=0; i<data.s.length; i++) {
				for(j=0; j<data.s.length; j++) {
					if(data.p[i][j] != 0) {
						this.particleSystem[id].addEdge((i+1), (j+1), {p: parseFloat(data.p[i][j])});
					}
				}
			}
		  /*	for(i=0; data.l.length; i++) {
				this.particleSystem[id].addNode(i+1, {lambda:data.lambda[i], mu:data.mu[i], s:data.s[i], l: data.l[i], lq:data.lq[i], w:data.w[i], wq: data.wq[i]});
			}*/
		} 
    },
	_Renderer: function(canvas){
		var canvas = $(canvas).get(0)
		var ctx = canvas.getContext("2d");
		var particleSystem

		var that = {
		  init:function(system){
			//
			// the particle system will call the init function once, right before the
			// first frame is to be drawn. it's a good place to set up the canvas and
			// to pass the canvas size to the particle system
			//
			// save a reference to the particle system for use in the .redraw() loop
			particleSystem = system

			// inform the system of the screen dimensions so it can map coords for us.
			// if the canvas is ever resized, screenSize should be called again with
			// the new dimensions
			particleSystem.screenSize(canvas.width, canvas.height) 
			particleSystem.screenPadding(80) // leave an extra 80px of whitespace per side
			
			// set up some event handlers to allow for node-dragging
			that.initMouseHandling()
		  },
		  
		  redraw:function(){
			// 
			// redraw will be called repeatedly during the run whenever the node positions
			// change. the new positions for the nodes can be accessed by looking at the
			// .p attribute of a given node. however the p.x & p.y values are in the coordinates
			// of the particle system rather than the screen. you can either map them to
			// the screen yourself, or use the convenience iterators .eachNode (and .eachEdge)
			// which allow you to step through the actual node objects but also pass an
			// x,y point in the screen's coordinate system
			// 
			ctx.fillStyle = "white";
			ctx.fillRect(0,0, canvas.width, canvas.height);
			var radius = 40;
			var perpendicularVector = function(pt1, pt2, length) {
				var aux = (((-pt2)*(-pt2))/((pt1*pt1)))+1;
				var y = Math.sqrt((length*length)/aux);
				var x = (-pt2*y)/pt1;
				return {y: y,
						x: x}
			}
			//Funcion que calcula el punto a length distancia del punto de origen en la recta con m de pendiente
			var pointinline = function(orign, m, length, dir) {
				var x = dir*Math.sqrt(Math.pow(length, 2)/(Math.pow(m,2)+1));
				var y = m*x;
				//Desplazamos al punto de origen
				return {x: x+orign.x, y: y+orign.y};
			}
			particleSystem.eachEdge(function(edge, pt1, pt2){
			  // edge: {source:Node, target:Node, length:#, data:{}}
			  // pt1:  {x:#, y:#}  source position in screen coords
			  // pt2:  {x:#, y:#}  target position in screen coords

			  // draw a line from pt1 to pt2
				ctx.strokeStyle = "rgba(0,0,0, 1)";
				ctx.lineWidth = 2;
			  /*var otherEdges = particleSystem.getEdgesFrom(edge.target)
			  var HasConexion = false;
			  for(i=0; i<otherEdges.length; i++)
				if (otherEdges[i].target == edge.source) {
					HasConexion = true;
					break;
				}*/
				//
				//ctx.quadraticCurveTo(pt1.x+radius, pt1.y-2.5*radius, ptcentral.x+cos, ptcentral.y-sen);
				//ctx.arc(ptcentral.x, ptcentral.y-1.25*radius, radius, 0.75*Math.PI, 0.25*Math.PI);
				//ctx.beginPath();
				if (edge.source.name == edge.target.name) {
					ctx.beginPath();
					ctx.arc(pt1.x, pt1.y-1.25*radius, radius, 0.75*Math.PI, 0.25*Math.PI);	
					ctx.stroke();
					ctx.fillStyle = "black";
					var sizeText = ctx.measureText(edge.data.p);
					ctx.fillText(edge.data.p, pt1.x-sizeText.width/2, pt1.y-2.4*radius);
					
				} else {
					if (parseInt(edge.source.name) > parseInt(edge.target.name)) {
						var perp = perpendicularVector((pt2.x-pt1.x), (pt2.y-pt1.y), 20);
						ctx.beginPath();
						ctx.moveTo(perp.x+pt1.x, perp.y+pt1.y);
						ctx.lineTo(perp.x+pt2.x, perp.y+pt2.y);
						ctx.stroke();
						var pMedio = {x: ((2*perp.x+pt2.x+pt1.x)/2), y: ((2*perp.y+pt2.y+pt1.y)/2)};
						//Dibujamos la flecha
						var sizeText = ctx.measureText(edge.data.p);
						if (pt2.x > pt1.x) {
							var endArrow = pointinline(pMedio, (pt2.y-pt1.y)/(pt2.x-pt1.x), 2*sizeText.width, 1);
							var startArrow = pointinline(pMedio, (pt2.y-pt1.y)/(pt2.x-pt1.x), sizeText.width, -1);
						} else { 
							var endArrow = pointinline(pMedio, (pt1.y-pt2.y)/(pt1.x-pt2.x), 2*sizeText.width, -1);
							var startArrow = pointinline(pMedio, (pt1.y-pt2.y)/(pt1.x-pt2.x), sizeText.width, 1);
						}
						ctx.beginPath();
						ctx.moveTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
						ctx.lineTo(perp.x+startArrow.x, perp.y+startArrow.y);
						ctx.lineTo(endArrow.x, endArrow.y);
						ctx.lineTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
						//ctx.arc(startArrow.x, startArrow.y, sizeText.width, 0, 2*Math.PI);
						ctx.fillStyle = "white";
						ctx.fill();
						ctx.stroke();
						ctx.fillStyle = "black";
						ctx.fillText(edge.data.p, pMedio.x-sizeText.width/2, pMedio.y);
						
					} else {
					    var perp = perpendicularVector((pt2.x-pt1.x), (pt2.y-pt1.y), 20);
						ctx.beginPath();
						ctx.moveTo((-perp.x)+pt1.x, (-perp.y)+pt1.y);
						ctx.lineTo((-perp.x)+pt2.x, (-perp.y)+pt2.y);
						ctx.stroke();
						var pMedio = {x: ((2*-perp.x+pt2.x+pt1.x)/2), y: ((2*-perp.y+pt2.y+pt1.y)/2)};
						//Dibujamos la flecha
						var sizeText = ctx.measureText(edge.data.p);
						if (pt2.x > pt1.x) {
							var endArrow = pointinline(pMedio, (pt2.y-pt1.y)/(pt2.x-pt1.x), 2*sizeText.width, 1);
							var startArrow = pointinline(pMedio, (pt2.y-pt1.y)/(pt2.x-pt1.x), sizeText.width, -1);
						} else { 
							var endArrow = pointinline(pMedio, (pt1.y-pt2.y)/(pt1.x-pt2.x), 2*sizeText.width, -1);
							var startArrow = pointinline(pMedio, (pt1.y-pt2.y)/(pt1.x-pt2.x), sizeText.width, 1);
						}
						ctx.beginPath();
						ctx.moveTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
						ctx.lineTo(perp.x+startArrow.x, perp.y+startArrow.y);
						ctx.lineTo(endArrow.x, endArrow.y);
						ctx.lineTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
						//ctx.arc(startArrow.x, startArrow.y, sizeText.width, 0, 2*Math.PI);
						ctx.fillStyle = "white";
						ctx.fill();
						ctx.stroke();
						ctx.fillStyle = "black";
						ctx.fillText(edge.data.p, pMedio.x-sizeText.width/2, pMedio.y);
					}
				}
				
			})

			particleSystem.eachNode(function(node, pt){
			  // node: {mass:#, p:{x,y}, name:"", data:{}}
			  // pt:   {x:#, y:#}  node position in screen coords

			  // draw a circle centered at pt
				ctx.beginPath();
				ctx.arc(pt.x, pt.y, radius, 0, 2 * Math.PI, false);
				ctx.fillStyle = (node.data.alone) ? "orange" : "white"
				ctx.fill();
				ctx.fillStyle = "black";
				var sizeText = ctx.measureText("M/M/"+node.data.s);
				ctx.fillText("Node " + node.name, pt.x-ctx.measureText("Node " + node.name).width/2, pt.y-15);
				ctx.fillText("M/M/"+node.data.s, pt.x - sizeText.width/2, pt.y);
				ctx.lineWidth = 1;
				ctx.strokeStyle = '#000000';
				ctx.stroke();
				
			})    			
		  },
		  
		  initMouseHandling:function(){
			// no-nonsense drag and drop (thanks springy.js)
			var dragged = null;

			// set up a handler object that will initially listen for mousedowns then
			// for moves and mouseups while dragging
			var handler = {
				moved:function(e){
					var pos = $(canvas).offset();
					_mouseP = arbor.Point(e.pageX-pos.left, e.pageY-pos.top);
					nearest = particleSystem.nearest(_mouseP);
					
					if (!nearest.node) return false;
					if (nearest.distance < 50) 
						console.log("Name: " + nearest.node.name);
				},
			  clicked:function(e){
				var pos = $(canvas).offset();
				_mouseP = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)
				dragged = particleSystem.nearest(_mouseP);

				if (dragged && dragged.node !== null){
				  // while we're dragging, don't let physics move the node
				  //dragged.node.fixed = true
				  particleSystem.eachNode(function(node, pt) {
						node.fixed=true;
				  });
				}

				$(canvas).bind('mousemove', handler.dragged)
				$(window).bind('mouseup', handler.dropped)

				return false
			  },
			  dragged:function(e){
				var pos = $(canvas).offset();
				var s = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)
				if (dragged && dragged.node !== null){
				  var p = particleSystem.fromScreen(s)
				  dragged.node.p = p
				}

				return false
			  },

			  dropped:function(e){
				if (dragged===null || dragged.node===undefined) return
				if (dragged.node !== null) dragged.node.fixed = false
				dragged.node.tempMass = 1000
				dragged = null
				$(canvas).unbind('mousemove', handler.dragged)
				$(window).unbind('mouseup', handler.dropped)
				_mouseP = null
				return false
			  }
			}
			
			// start listening
			$(canvas).mousedown(handler.clicked);
			$(canvas).mousemove(handler.moved);
		  },
		  
		}
		return that
	  }    
});
Shiny.outputBindings.register(networkOutputBinding, 'shiny.networkOutput');

/*//Creamos un InputBinding para introducir los grafos de conexion
var graphInputBinding = new Shiny.InputBinding();
$.extend(graphInputBinding, {
	find: function(scope) {
		return $(scope).find('.shiny-graph-input');
	},
	getId: function(el) {
		return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
	},
	getValue: function(el) {
		return{
			matrix: $(el).attr('data-graph')
		}
	},
	setValue: function(el, value) {
	},
	subscribe: function(el, callback) {
		var id = $(el).attr("id");
		var sys = this.sys[id];
		var w = $("#" + id + "-graphcanvas").attr("width");
		var h = $("#" + id + "-graphcanvas").attr("height");
		
		$("#"+id+"-numNodes").on("change", function(event) {
			sys.eachNode(function(node, pt) {
				sys.pruneNode(node);
			});
			nNodes = $("#"+id+"-numNodes").val();
			for(var i=1; i<=nNodes; i++) {
				sys.addNode(i);
			}
		});
		
		$(el).on("ininodes", function(event) {
		    nNodes = $("#"+id+"-numNodes").val();
		    for(var i=1; i<=nNodes; i++) {
				console.log("x: " + i*w/nNodes + ", y: " + i*h/nNodes);
				this.sys[id].addNode(i, {fixed: true, x:i*w/nNodes, y:i*h/nNodes});
			}
		});
	},
	initialize: function(el) {		
		if ($(el).attr("data-graph") == null)
			$(el).attr("data-graph", "[]");
		
		if (!this.hasOwnProperty("sys"))
			this.sys = new Array();
		
		var id = $(el).attr("id");
		$(el).append("<button id='"+ id +"-manualInput'>Manual Input</button>" +
					  "<div id='"+ id +"-manualDialog'>" +
							"<label for='" + id + "-numNodes'>Number of nodes: </label><input id='"+ id +"-numNodes' type=number value=5 min=1 max=10/>" +
							"<canvas id='" + id + "-graphcanvas' width=565 height=440></canvas>" +
					  "</div>");
		$("#" + id + "-manualDialog").dialog({
		    autoOpen: false,
			resizable: false,
			height: 650,
			width: 600,
			modal: true,
			buttons: {
				Ok: function() {
					$(el).trigger("send");
					$(this).dialog("close");
				},
				Cancel: function() {
					$(this).dialog("close")
				}
			}
		});
		
		$("#" + id + "-manualInput").button().click(function() {
			$("#" + id + "-manualDialog").dialog("open");
		});
		this.idCounter++;
		
		this.sys[id] = arbor.ParticleSystem(); // create the system with sensible repulsion/stiffness/friction
		this.sys[id].renderer = this._Renderer("#" + id + "-graphcanvas"); // our newly created renderer will have its .init() method called shortly by sys...
	    for(var i=1; i<=$("#"+id+"-numNodes").val(); i++) {
				this.sys[id].addNode(i);
			}
	},
	_Renderer: function(canvas){
		var canvas = $(canvas).get(0)
		var ctx = canvas.getContext("2d");
		var particleSystem

		var that = {
		  init:function(system){
			//
			// the particle system will call the init function once, right before the
			// first frame is to be drawn. it's a good place to set up the canvas and
			// to pass the canvas size to the particle system
			//
			// save a reference to the particle system for use in the .redraw() loop
			particleSystem = system

			// inform the system of the screen dimensions so it can map coords for us.
			// if the canvas is ever resized, screenSize should be called again with
			// the new dimensions
			particleSystem.screenSize(canvas.width, canvas.height) 
			particleSystem.screenPadding(80) // leave an extra 80px of whitespace per side
			
			// set up some event handlers to allow for node-dragging
			that.initMouseHandling()
		  },
		  
		  redraw:function(){
			// 
			// redraw will be called repeatedly during the run whenever the node positions
			// change. the new positions for the nodes can be accessed by looking at the
			// .p attribute of a given node. however the p.x & p.y values are in the coordinates
			// of the particle system rather than the screen. you can either map them to
			// the screen yourself, or use the convenience iterators .eachNode (and .eachEdge)
			// which allow you to step through the actual node objects but also pass an
			// x,y point in the screen's coordinate system
			// 
			ctx.fillStyle = "white"
			ctx.fillRect(0,0, canvas.width, canvas.height)
			
			particleSystem.eachEdge(function(edge, pt1, pt2){
			  // edge: {source:Node, target:Node, length:#, data:{}}
			  // pt1:  {x:#, y:#}  source position in screen coords
			  // pt2:  {x:#, y:#}  target position in screen coords

			  // draw a line from pt1 to pt2
			  ctx.strokeStyle = "rgba(0,0,0, .333)"
			  ctx.lineWidth = 1
			  ctx.beginPath()
			  ctx.moveTo(pt1.x, pt1.y)
			  ctx.lineTo(pt2.x, pt2.y)
			  ctx.stroke()
			})

			particleSystem.eachNode(function(node, pt){
			  // node: {mass:#, p:{x,y}, name:"", data:{}}
			  // pt:   {x:#, y:#}  node position in screen coords

			  // draw a circle centered at pt
			  var w = 10
			  ctx.beginPath();
			  ctx.arc(pt.x-w/2, pt.y-w/2, 20, 0, 2 * Math.PI, false);
			  ctx.fillStyle = (node.data.alone) ? "orange" : "white"
			  ctx.fill();
			  ctx.fillStyle = "black";
			  var sizeText = ctx.measureText(node.name);
			  ctx.fillText(node.name, pt.x-w/2 - sizeText.width/2, pt.y-w/2);
			  ctx.lineWidth = 2;
			  ctx.strokeStyle = '#000000';
			  ctx.stroke();
			  //ctx.fillRect(pt.x-w/2, pt.y-w/2, w,w)
			})    			
		  },
		  
		  initMouseHandling:function(){
			// no-nonsense drag and drop (thanks springy.js)
			var dragged = null;

			// set up a handler object that will initially listen for mousedowns then
			// for moves and mouseups while dragging
			var handler = {
			  clicked:function(e){
				var pos = $(canvas).offset();
				_mouseP = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)
				dragged = particleSystem.nearest(_mouseP);

				if (dragged && dragged.node !== null){
				  // while we're dragging, don't let physics move the node
				  //dragged.node.fixed = true
				  particleSystem.eachNode(function(node, pt) {
						node.fixed=true;
				  });
				}

				$(canvas).bind('mousemove', handler.dragged)
				$(window).bind('mouseup', handler.dropped)

				return false
			  },
			  dragged:function(e){
				var pos = $(canvas).offset();
				var s = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)
				if (dragged && dragged.node !== null){
				  var p = particleSystem.fromScreen(s)
				  dragged.node.p = p
				}

				return false
			  },

			  dropped:function(e){
				if (dragged===null || dragged.node===undefined) return
				if (dragged.node !== null) dragged.node.fixed = false
				dragged.node.tempMass = 1000
				dragged = null
				$(canvas).unbind('mousemove', handler.dragged)
				$(window).unbind('mouseup', handler.dropped)
				_mouseP = null
				return false
			  }
			}
			
			// start listening
			$(canvas).mousedown(handler.clicked);

		  },
		  
		}
		return that
	  }    
});
Shiny.inputBindings.register(graphInputBinding, 'shiny.graphInputBinding');*/

//Creamos un InputBinding para el menu de Jquery UI
 var menuInputBinding = new Shiny.InputBinding();
  $.extend(menuInputBinding, {
	find: function(scope) {
		return $(scope).find('.shiny-menu-input');
	},
	getId: function(el) {
		return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
	},
	getValue: function(el) {
		return {menu: this.menu,
				clicked: this.clicked,
				selected: this.selected};
	},
	setValue: function(el, value) {
	},
	subscribe: function(el, callback) {
		var self = this;
		$(el).on('change.menuInputBinding', function(event) {
			var $el = $(el);
			$el.empty();
			
			var writeMenu = function(data) {
				var aux = "";
				for(var i=0; i<data.length; i++) {
					aux += "<li><a idMenu='"+ data[i].id +"' href='#'>" + data[i].title + "</a>\n"
					if (data[i].submenu.length > 0)
						aux += "<ul>" + writeMenu(data[i].submenu) + "</ul>\n";
					aux += "</li>";
				}
				return aux;
			};
			$el.html(writeMenu(self.menu));
			
			$el.find("a[idMenu]").each(function() {
				$(this).on("click", function(event) {
					self.selected = $(this).attr("idMenu");
					self.clicked += 1;
					callback(true);
				});
			});
			self.clicked += 1;
			$el.menu("refresh");
			callback(true);
		});
	},
	unsubscribe: function(el) {
		$(el).off(".menuInputBinding");
	},
	receiveMessage: function(el, data) {
		var $el = $(el);
		if (data.hasOwnProperty("action")) {
			if (data.action == "set") {
				if (data.hasOwnProperty("menu")) {
					this.menu = this._buildMenu(data.menu);
					}
				}
		}
		$el.trigger("change");
	},
	getState: function(el) {
	},
	getRatePolicy: function() {
		return {
			policy: 'debounce',
			delay: 250
		};
	},
	
	initialize: function(el) {
		this.menu = new Array();
		this.selected = 0;
		this.clicked = 0;
		$(el).empty();
		$(el).menu();
	},
	
	_buildMenu: function(data) {
		var menu = new Array();
		for(var i=0; i < data.length; i++) {
			var title = data[i].title;
			if (data[i].submenu.length > 0)
				var submenu = this._buildMenu(data[i].submenu);
			else
				var submenu = new Array();
			menu.push({title: title, submenu: submenu, id: data[i].id});
		}
		return menu;
 	}
  });
  Shiny.inputBindings.register(menuInputBinding, 'shiny.menuInputBinding');
  
   //Creamos un objeto que guarde los valores más importantes de un Tab de Jquery UI
 var JqueryUITab = function () {
	this.total = 0;
	this.size = 0;
	
	this.tabAdded = function() {
		this.size++;
		this.total++;
	};
	
	this.tabRemoved = function() {
		this.size--;
	};
 };
 
  //Creamos un InputBinding para las tabs de Jquery UI
  var tabInputBinding = new Shiny.InputBinding();
  $.extend(tabInputBinding, {
	find: function(scope) {
		return $(scope).find('.shiny-tabs-input');
	},
	getId: function(el) {
		return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
	},
	getValue: function(el) {			
		return {
			selected: $(el).tabs('option', 'active'),
			total: this.infoTabs[$(el).attr("id")].total,
			size: this.infoTabs[$(el).attr("id")].size
		}
	},
	setValue: function(el, value) {
		$(el).tabs("option", 'active', value);
	},
	
	subscribe: function(el, callback) {
		var self = this;
		$(el).on('updateTabsAdd.tabInputBinding', function(event) {
			Shiny.unbindAll();
			$(el).tabs("refresh");
			$(el).tabs("option", "active", self.infoTabs[$(el).attr("id")].size - 1);
			Shiny.bindAll();
			callback(false);
		});
		
		$(el).on('updateTabsRemove.tabInputBinding', function(event) {
			$(el).tabs("refresh");
			callback(true);
		});
		
		$(el).on('clicktab', function(event) {
			callback(false);
		});
	},
	unsubscribe: function(el) {
		$(el).off(".tabInputBinding");
	},
	receiveMessage: function(el, data) {
		var $el = $(el);
		var self = this;
		if (data.hasOwnProperty("action")) {
			if (data.action == "add") {
				if (data.hasOwnProperty("value") && data.hasOwnProperty("removeButton")) {
					var $ul = $el.find("ul:first");
					for(var i=0; i < data.value.length; i++) {
						tabObject = self.infoTabs[$el.attr("id")];
						tabObject.tabAdded();
						
						var newTab = "<li><a href='#"+ $el.attr("id") +"-tag-"+ tabObject.total + "'>" + data.value[i].title + "</a>";
						if (data.removeButton)  {
							newTab += "<span class='ui-icon ui-icon-close' role='presentation'>Remove Tab</span></li>\n";
							$ul.append(newTab);
							
							$ul.find("span:last").each(function() {
								$(this).on("click", function(event) {
									var panelId = $( this ).parent().remove().attr( "aria-controls" );
									$( "#" + panelId ).remove();
									tabObject.tabRemoved();
									$el.trigger('updateTabsRemove');
								});
							});
						}
						else {
							newTab += "</li>\n";
							$ul.append(newTab);
						}
							
						//$ul.append("<li><a href='#"+ $el.attr("id") +"-tag-"+ tabObject.total + "'>" + data.value[i].title + "</a><span class='ui-icon ui-icon-close' role='presentation'>Remove Tab</span></li>\n");
						$ul.find("a:last").each(function() {
							$(this).on("click", function(event) {
								$el.trigger("clicktab");
							});
						});
						
						$el.append("<div id='"+ $el.attr("id") +"-tag-"+ tabObject.total + "'>" + data.value[i].content + "</div>\n");
						Shiny.initializeInputs($("#"+$el.attr("id") +"-tag-"+ tabObject.total));
						$el.trigger('updateTabsAdd');
					}	
				}	
			} else if (data.action == "reload") {
				self._reloadTabs($el);
			}
		}
	},
	getState: function(el) {
		//Unused
	},
	getRatePolicy: function() {
		return {
			policy: 'debounce',
			delay: 250
		};
	},
	initialize: function(el) {
		if(!this.hasOwnProperty("infoTabs"))
			this.infoTabs = new Array();
			
	    //Si no está inicializado el elemento lo hacemos
		if (!this.infoTabs.hasOwnProperty($(el).attr("id"))) {
			this.infoTabs[$(el).attr("id")] = new JqueryUITab();
			
			if ($(el).find("ul:first").length == 0)
				$(el).append("<ul></ul>");
			$(el).tabs();
			$(el).find( ".ui-tabs-nav" ).sortable({
				axis: "x",
				stop: function() {
					$(el).tabs( "refresh" );
				}
			});
		}
	},
	_reloadTabs: function (el) {
		var $el = $(el);
		$el.find("ul").empty();
		var newTitles = "";
		for(var i=0; i<self.title.length; i++) {
			newTitles += "<li><a href='#tag-"+ (i+1) + "'>" + self.title[i] + "</a><span class='ui-icon ui-icon-close' role='presentation'>Remove Tab</span></li>\n";
		}
		$el.find("ul").append(newTitles);
			
		$el.find("a").each(function() {
			$(this).on("click", function(event) {
				$el.trigger('clicktab');
			});
		});
			
		$el.find("span").each(function() {
			$(this).on("click", function(event) {
				var panelId = $( this ).parent().attr("aria-controls").replace("tag-", "");
				self.title.splice(panelId-1, 1);
				self.content.splice(panelId-1, 1);
				$(el).trigger('updateTabs');
			});
		});
		
		$el.find("div").remove();
		var newDivs = "";
		for(var i=0; i<self.content.length; i++) {
			newDivs += "<div id='tag-"+ (i+1) + "'>" + self.content[i] + "</div>\n";
		}
		$el.append(newDivs);
		$el.tabs("refresh");
		$el.tabs("option", 'active', self.title.length-1);
	}
  });
  Shiny.inputBindings.register(tabInputBinding, 'shiny.tabInput');
  
   //Creamos un InputBinding para los sliders de Jquery UI
 var JqueryUIsliderInputBinding = new Shiny.InputBinding();
 $.extend(JqueryUIsliderInputBinding, {
	find: function(scope) {
			return $(scope).find('.shiny-slider-input');
	},
	getId: function(el) {
		return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
	},
	getValue: function(el) {			
		return {
			values: $(el).slider( "option", "values"),
			min: $(el).slider( "option", "min"),
			max: $(el).slider( "option", "max"),
			step: $(el).slider("option", "step")
		}
	},
	setValue: function(el, value) {
		$(el).slider("option", 'values', value);
	},
	subscribe: function(el, callback) {
		var self = this;
		$(el).on("slide.JqueryUIsliderInputBinding", function( event, ui ) {
			callback(false);
		});

		$(el).on("change.JqueryUIsliderInputBinding", function(event) {
			callback(true);
		});
	},
	unsubscribe: function(el) {
		$(el).off(".JqueryUIsliderInputBinding");
	},
	receiveMessage: function(el, data) {
		var $el = $(el);
		var self = this;
		
		var ran = $(el).slider("option", "range");
		var min =  $(el).slider("option", "min");
		var max =  $(el).slider("option", "max");
		var step =  $(el).slider("option", "step");
		var values =  $(el).slider("option", "values");
		
		if (data.hasOwnProperty("values"))
			if (data.values != null)
				values = data.values;
		
		if (data.hasOwnProperty("max"))
			if (data.max != null)
				max = data.max
		
		if (data.hasOwnProperty("min"))
			if (data.min != null)
				min = data.min
			
		if (data.hasOwnProperty("step"))
			if (data.step != null)
				step = data.step

		console.log(ran + ", " + min + ", " + max + ", " + step + ", " + values);
		$(el).slider("destroy");
		$(el).slider({
			range:ran,
			min: min,
			max: max,
			step: step,
			values: values
		});
		$el.trigger("change.JqueryUIsliderInputBinding");
	},
	getState: function(el) {
		//Unused
	},
	getRatePolicy: function() {
		return {
			policy: 'debounce',
			delay: 250
		};
	},
	initialize: function(el) {
		var values = $(el).attr("values");
		
		$(el).slider({
			range: Boolean($(el).attr("range")),
			min: parseFloat($(el).attr("min")),
			max: parseFloat($(el).attr("max")),
			step: parseFloat($(el).attr("step")),
			values: values.substr(1, values.length-2).split(",").map(Number)
		});
		
		$(".ui-slider-horizontal").width("95%");
	}
  });
  Shiny.inputBindings.register(JqueryUIsliderInputBinding, 'shiny.JqueryUIsliderInput');