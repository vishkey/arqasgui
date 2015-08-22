String.prototype.capitalize = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
};

String.prototype.endsWith = function(suffix) {
    return this.indexOf(suffix, this.length - suffix.length) !== -1;
};

$.fn.scrollTo = function( target, options, callback ){
  if(typeof options == 'function' && arguments.length == 2){ callback = options; options = target; }
  var settings = $.extend({
    scrollTarget  : target,
    offsetTop     : 50,
    duration      : 500,
    easing        : 'swing'
  }, options);
  return this.each(function(){
    var scrollPane = $(this);
    var scrollTarget = (typeof settings.scrollTarget == "number") ? settings.scrollTarget : $(settings.scrollTarget);
    var scrollY = (typeof scrollTarget == "number") ? scrollTarget : scrollTarget.offset().top + scrollPane.scrollTop() - parseInt(settings.offsetTop);
    scrollPane.animate({scrollTop : scrollY }, parseInt(settings.duration), settings.easing, function(){
      if (typeof callback == 'function') { callback.call(this); }
    });
  });
}

var myNumberInputBinding = new Shiny.InputBinding();
$.extend(myNumberInputBinding, {
    find: function(scope) {
      return $(scope).find('.myNumberInputBinding');
    },
    getId: function(el) {
      return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function(el) {
      var numberVal = $(el).val();
      if (/^\s*$/.test(numberVal))  // Return null if all whitespace
        return null;
      else if (!isNaN(numberVal))   // If valid Javascript number string, coerce to number
        return +numberVal;
      else
        return numberVal;           // If other string like "1e6", send it unchanged
    },
    setValue: function(el, value) {
      el.value = value;
    },
    getType: function(el) {
      return "shiny.number";
    },
    subscribe: function(el, callback) {
      $(el).on('keyup.textInputBinding input.textInputBinding', function(event) {
        callback(true);
      });
      $(el).on('change.textInputBinding', function(event) {
        callback(true);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.textInputBinding');
    },
    receiveMessage: function(el, data) {
      console.log(data);
      if (data.hasOwnProperty('attr') & data.hasOwnProperty('attrValue'))
          $(el).attr(data.attr, data.attrValue);
      if (data.hasOwnProperty('value'))  el.value = data.value;
      if (data.hasOwnProperty('min'))    el.min   = data.min;
      if (data.hasOwnProperty('max'))    el.max   = data.max;
      if (data.hasOwnProperty('step'))   el.step  = data.step;

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    getState: function(el) {
      return { label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
               value: this.getValue(el),
               min:   Number(el.min),
               max:   Number(el.max),
               step:  Number(el.step) };
    },
    getRatePolicy: function() {
      return {
        policy: 'debounce',
        delay: 500
      };
    },
    initialize: function(el) {
      console.log($(el).attr("id"));
    }
});

Shiny.inputBindings.register(myNumberInputBinding, 'shiny.myNumberInputBinding');

var JQueryActionButtonInputBinding = new Shiny.InputBinding();
$.extend(JQueryActionButtonInputBinding, {
	find: function(scope) {
      return $(scope).find(".shiny-jquerybutton-input");
    },
    getValue: function(el) {
      if ($(el).is("[dialog-input]")) {
        var res = new Object();
        res.value = $(el).data('val');
        res.name  = $(el).data('name');
        res.distr = $(el).data('distribution');
        return(res);
      }
      return $(el).data('val');
    },
    setValue: function(el, value) {
    },
    subscribe: function(el, callback) {
      if ($(el).is("[dialog-input]")) {
        $(el).parent().append("<div id='"+ $(el).attr("id") + "_dialog' title='Insert a name for the distribution'><center><label>Name: </label><input type='text' id='"+ $(el).attr("id") + "_input'/></center></div>");
        $("#" + $(el).attr("id") + "_dialog").dialog({
            autoOpen: false,
        		resizable: false,
      			modal: true,
            width: 330,
      			buttons: {
      				Ok: function() {
      					$(el).trigger("send.JQueryActionButtonInputBinding");
      					$(this).dialog("close");
      				},
      				Cancel: function() {
                $(el).trigger("cancel.JQueryActionButtonInputBinding")
      					$(this).dialog("close");
      				}
		    	  }
	      	});
          if ($(el).data("val") == null)
            $(el).data("val", 0);
      }
      
      $(el).on("send.JQueryActionButtonInputBinding", function(e) {
        e.stopImmediatePropagation();
        $(el).data('name', $("#"+ $(el).attr("id") + "_input").val());
        callback();
      })
      
      $(el).on("cancel.JQueryActionButtonInputBinding", function(e) {
        e.stopImmediatePropagation();
        $("#" + $(el).attr("id") + "_input").val("");
        $(el).removeAttr("disabled");
      })
      
      $(el).on("click.JQueryActionButtonInputBinding", function(e) {
        e.stopImmediatePropagation();
        if (!$(el).is("[disabled]")) {
          var $el = $(this);
          var val = $el.data('val') ;
          $el.data('val', val + 1);
          if ($el.attr("once") != null) {
            $(el).attr("disabled", "");
          }
          
          if ($el.is("[dialog-input]")) {
            $("#"+ $el.attr("id") + "_dialog").dialog("open");
            return;
          }else {  
            callback();
          }
        }
      });
    },
    getState: function(el) {
       if ($(el).is("[dialog-input]")) {
        var res = new Object();
        res.value = $(el).data('val');
        res.name  = $(el).data('name');
        res.distr = $(el).data('distribution');
        return(res);
      }
      return $(el).data('val');
    },
    receiveMessage: function(el, data) {
        if (data.hasOwnProperty("disabled")) {
          if (data.disabled) {
            $(el).attr("disabled", "");
          }else{
            $(el).removeAttr("disabled");
          }
        }
    },
    unsubscribe: function(el) {
      $(el).off(".JQueryActionButtonInputBinding");
    },
	initialize: function(el) {
      $(el).button();
      $(el).data("val", 0);
   }
});
Shiny.inputBindings.register(JQueryActionButtonInputBinding, 'shiny.JQueryActionButtonInput');

var reportDatabase = function () {
	this.reports = new Array();
  
	this.addReport = function(menu, body) {
    this.reports.push({menu:menu, body:body});
	};
	
	this.removeReport = function(index) {
   this.reports.remove(index); 
	};
  
  this.length = function() {
    return(this.reports.length);
  }
};

var SessionReports = new reportDatabase();

var JQuerySaveReportInputBinding = new Shiny.InputBinding();
$.extend(JQuerySaveReportInputBinding, {
  find: function(scope) {
      return $(scope).find(".shiny-savereport-input");
    },
    getValue: function(el) {
      var id = $(el).attr("id");
      var res = new Array();
      $("#"+id + "-dialog").find(":checked").each(function(i, v) {
        var val = $(v).attr("value");
        var data = new Object();
        var numTab = $(el).attr("id");
        numTab = numTab.replace(/[^0-9]+/ig,"");
        switch(val) {
          case "probabilities" : {if ($("#PnQnMin"+ numTab).length == 0) {
                                    data.from=parseInt($("#PnMin"+numTab).val());
                                    data.to = parseInt($("#PnMax"+numTab).val());
                                  }
                                  else {
                                    data.from=parseInt($("#PnQnMin"+numTab).val()); 
                                    data.to=parseInt($("#PnQnMax"+numTab).val());
                                  }
                                  break;}
          case "waitingtimes": {data.from=parseFloat($("#WtWqtMin"+numTab).val());
                                data.to=parseFloat($("#WtWqtMax"+numTab).val());
                                data.step=parseFloat($("#WtWqtStep"+numTab).val());
                                break;}
          case "probabilitiesplots": {data.from=parseInt($("#PnQnMin"+numTab).val()); 
                                      data.to=parseInt($("#PnQnMax"+numTab).val());
                                      break;}
          case "waitingplots" : {data.from=parseFloat($("#WtWqtMin"+numTab).val());
                                 data.to=parseFloat($("#WtWqtMax"+numTab).val());
                                 data.step=parseFloat($("#WtWqtStep"+numTab).val());
                                 break;}
          case "combinedprobabilities" : {var combprobs = $("#pn1nk"+ numTab).val().replace(/;/gi, ",");
                                          data.combinedprobabilities = JSON.parse("[" + combprobs + "]");
                                         break;}
          case "networkgraph" : {data.image = $("#networkDiv"+ numTab+"-graphcanvas")[0].toDataURL("image/png");
                                 break;}
          case "LEvolution" : {data.depth = parseInt($("#depth" + numTab).val()); break;}
          case "LqEvolution": {data.depth = parseInt($("#depth" + numTab).val()); break;}
          case "WEvolution" : {data.depth = parseInt($("#depth" + numTab).val()); break;}
          case "WqEvolution": {data.depth = parseInt($("#depth" + numTab).val()); break;}
          case "ClientsEvolution": {data.depth = parseInt($("#depth" + numTab).val());
                                    data.nsim = parseInt($("#simulationInput" + numTab).val()); break;}
          case "IntensityEvolution": {data.depth = parseInt($("#depth" + numTab).val()); break;}
          default: {data = null; break;}

        }
        res.push({val:val, data: data});
      });
      return({title: $("#"+id+"-title").val(), checkboxes:res, button: $(el).data("val"), nReports: SessionReports.length()});
    },
    setValue: function(el, value) {
    },
    subscribe: function(el, callback) {
      var id = $(el).attr("id");
      $(el).on("send.JQuerySaveReportInputBinding", function() {
          $(el).data("val", parseInt($(el).data("val"))+1);
          callback(false);  
      });
      
      $("#"+id+"-button").on("click", function(){
          $("#" + id + "-dialog").dialog("open");
      });
    },
    getState: function(el) {
    },
    receiveMessage: function(el, data) {
      var id = $(el).attr("id");
      if ((data.action=="setSections") && data.hasOwnProperty("sections")) {
        var insert = "<form class='pure-form pure-form-stacked'><fieldset>";
        insert = insert + "<label for='" + id + "-title'>Title: </label><input id='" + id + "-title' type='text'></input>";
        for (i in data.sections)
          insert = insert + "<label for='"+ id +"-"+ i + "'><input id='"+ id +"-"+ i + "' type='checkbox' name='section' value='" + i + "'> "+ data.sections[i] +"</input></label>";
        insert = insert + "</fieldset></form>";
        $("#" + id + "-dialog").empty().append(insert);
      } else if (data.action == "saveReport") {
          SessionReports.addReport(data.html[0], data.html[1]);
      }

      if (data.disabled) {
        $("#"+id+"-button").attr("disabled", "");
      }else{
        $("#"+id+"-button").removeAttr("disabled");
      }
    },
    unsubscribe: function(el) {
      $(el).off(".JQuerySaveReportInputBinding");
    },
  	initialize: function(el) {
      var id = $(el).attr('id');
      $(el).data("val", 0);
      $(el).append("<div id='" + id + "-dialog'></div><button id='" + id + "-button' class='btn btn-primary'>Save report</button>");
        
        $("#" + id + "-button").button();
      $("#" + id + "-dialog").dialog({
            autoOpen: false,
            resizable: false,
            modal: true,
            width: 330,
      			buttons: {
      				Ok: function() {
      					$(el).trigger("send.JQuerySaveReportInputBinding");
      					$(this).dialog("close");
      				},
      				Cancel: function() {
                $(el).trigger("cancel.JQuerySaveReportInputBinding");
      					$(this).dialog("close");
      			  }	
      		}
		  });
    }
});
Shiny.inputBindings.register(JQuerySaveReportInputBinding, 'shiny.JQuerySaveReportInputBinding');

var checkboxVectorDistrInputBinding = new Shiny.InputBinding();
$.extend(checkboxVectorDistrInputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-checkboxvdistr-input');
    },
    getId: function(el) {
      return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function(el) {
      var id = $(el).attr("id");
      var res = new Array();
      $("#"+id).find(":checked").each(function(i, v) {
        res.push($(v).attr("value"));
      });
      return(res);
  	},
    setValue: function(el) {
    },
    subscribe: function(el, callback) { 
      var id = $(el).attr("id");
      var self = this;
  	  $("#"+id).on("click", function(event) {
  		  callback(true);
  	  });
    },
    unsuscribe: function(el) {
      
    },
    receiveMessage: function(el, data) {
      var id = $(el).attr("id");
    	if (data.hasOwnProperty('distributions')) {
        this.distributions[id] = data.distributions;
  			for (var i=0; i<data.distributions.length; i++) {
  				$("#"+id).append("<input type='checkbox' value='" + data.distributions[i] + "' checked>" + data.distributions[i] + "</input><br>");
  			}
        $("#"+id).trigger('click');
  		}
    },
    getState: function(el) {
    },
    getRatePolicy: function() {
		  return {
			  policy: 'debounce',
			  delay: 700
		  };
	  },
    initialize: function(el) {
      this.distributions = new Array();
    }
});
Shiny.inputBindings.register(checkboxVectorDistrInputBinding, 'shiny.checkboxVectorDistrInputBinding');

var selectVectorDistrInputBinding = new Shiny.InputBinding();
$.extend(selectVectorDistrInputBinding, {
  find: function(scope) {
      return $(scope).find('.shiny-vdistr-input');
    },
  getId: function(el) {
      return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
    },
	getValue: function(el) {
    var distributions = new Object();
    var id = $(el).attr("id");
    $("* [id^='"+ id +"-select-']").each(function(index) {
      var params = new Array();
		  $("#"+id+"-distrInputs-"+(index+1)).find("input[type=number]").each(function() {
			  params.push({name: $(this).attr("name"), value: $(this).val()});
		  });
      distributions[index] = {distribution: parseInt($(this).val())+1, params: params};
    });
    return(distributions);
	},
  setValue: function(el, value) {
  },
	subscribe: function(el, callback) {
    /*var id = $(el).attr("id");
    var self = this;
    $("* [id^='"+id+"-select-']").each(function(index) {
      $(this).on("change",{id:id, self:self, index:index},  self._selectChange);
    });*/
  
  $(el).on("send.selectVectorDistrInputBinding", function(event) {
    event.stopImmediatePropagation();
		callback(false);
	});
  },
  receiveMessage: function(el, data) {
    var id = $(el).attr("id");
    var self = this;
    if (!self.distributions.hasOwnProperty(id))
      self.distributions[id] = new Array();
    if (data.hasOwnProperty('numNodes')) {
        console.log("Prev: " + self.numNodes[id] + " New: " + data.numNodes);
        self._generateNodeSelects(el, self.numNodes[id], data.numNodes);
        self.numNodes[id] = data.numNodes;
    }  
  	if (data.hasOwnProperty('distributions')) {
      //Se guarda una copia de la distribuciones base para copiarlas en caso de ser necesario
      self.distributions[id].masterdistributions = data.distributions;
			for (var i=0; i<data.distributions.length; i++) {
				$("* [id^='"+id+"-select-']").each(function(index) {
            self.distributions[id][index]= $.extend(true, {}, data.distributions);
  			    $(this).append("<option value=" + i + ">" + data.distributions[i].name + "</option>");
			  });
			}
		}
    
    var defval = $(el).attr("defaultvalue");
    if (defval != null) {
      defval = eval("(" + defval + ")");
      for(i=0; i<defval.length; i++) {
        self.distributions[id][i][defval[i].id-1].params = defval[i].params;
        $("#"+id+"-select-"+(i+1)).val(defval[i].id-1);
      }
    }
		$("* [id^='"+id+"-select-']").each(function() {
        $(this).trigger('change');
  	});
    
    $(el).trigger("send");
	},
	getState: function(el) {
  },
	initialize: function(el) {
    var id = $(el).attr("id");
    this.distributions = new Array();
    this.numNodes = new Array();
    
    var defval = $(el).attr("defaultvalue");
    if (defval != null) {
      defval = eval("(" + defval + ")");
      this.numNodes[id] = defval.length;
    }
      
    $(el).append("<button id='"+ id +"-manualInput' class='btn btn-info'>Edit/View</button>" +
                  "<div id='"+ id + "-div'>" +
                    "<span></span>" + 
                  "</div>")
    $("#"+id+"-div").dialog({
  	    autoOpen: false,
			resizable: false,
			height: 600,
			width: 800,
			resizable: true,
			modal: true,
			buttons: {
				Ok: function() {
					$(el).trigger("send.selectVectorDistrInputBinding");
					$(this).dialog("close");
				},
				Cancel: function() {
					$(this).dialog("close");
				}
			}
		});
    
    $("#" + id + "-manualInput").button().click(function() {
  		$("#" + id + "-div").dialog("open");
		});
    if (this.numNodes[id] > 0) 
      this._generateNodeSelects(el, 0, this.numNodes[id])
    
  },
  _generateNodeSelects: function(el, prevval, newval) {
      var id = $(el).attr("id");
      var self = this;
      var distrzone = $("#"+ id + "-div").find("span");
      if (prevval == 0 || newval > prevval) {
        //Shiny.unbindAll(distrzone);
        for (i=(prevval+1); i<=newval; i++) {
          distrzone.append("<label for='"+ id + "-select-"+i+"'><u><b>Node "+ i +":</b></u></label><select id='" + id + "-select-"+i+"' style='display:block; margin-top:15px;'></select>").append("<div id='" + id + "-distrInputs-" + i +"' style='display:inline; height:auto; width:auto;'></div>");
          //Si hay cargada una lista de distribuciones (maestra, cargamos las <option> en el nuevo <select>
          if ((self.distributions[id] != undefined) && (self.distributions[id].hasOwnProperty("masterdistributions"))) {
            if (self.distributions[id][i-1] == undefined){
              self.distributions[id][i-1] = $.extend(true, {}, self.distributions[id].masterdistributions);
            }
            for(j=0; j<self.distributions[id].masterdistributions.length; j++) {
              $("#"+id+"-select-"+i).append("<option value="+j+">"+ self.distributions[id][i-1][j].name + "</option>");
            }
          }
        }
        Shiny.bindAll(distrzone);
        $("* [id^='"+id+"-select-']").each(function(index) {
          $(this).on("change",{id:id, self:self, index:index},  self._selectChange);
        });
    } else if (prevval > newval) {
        for (i=prevval; i>newval; i--) {
          $("#"+id+"-select-"+i).remove();
          $("#"+id+"-distrInputs-"+i).remove();
          $("label[for='" + id + "-select-"+ i +"']").remove();
        }
      }
  },
  _selectChange: function(event) {
        console.log(event.data.id);
        var id = event.data.id;
        var self = event.data.self;
        var index = event.data.index;
        
  	    event.stopImmediatePropagation();
        var paramDiv = $("#"+ id +"-distrInputs-" + (index+1));
        paramDiv.empty();
		    var selected = $(this).val();
		    var parameters = self.distributions[id][index][selected].params;
	  	  for(var i=0; i<parameters.length; i++) {
			    paramDiv.append("<label>"+parameters[i].name+ ": </label><input id='"+ id + "-distrInput-"+ (index+1) + "-" + parameters[i].name+ "' name='"+ parameters[i].name + "' value='"+ parameters[i].value +"' style='width:4.5em;' type='number'/><br><br>");
			    $("#"+id+"-distrInput-"+ (index+1)+"-"+parameters[i].name).on("change", {posparam:i, posselect:index}, function(event) {
            self.distributions[id][event.data.posselect][selected].params[event.data.posparam].value = $(this).val();
		      	});
		    }
	 }
});
Shiny.inputBindings.register(selectVectorDistrInputBinding, 'shiny.selectVectorDistrInput');

var selectDistrInputBinding = new Shiny.InputBinding();
$.extend(selectDistrInputBinding, {
	find: function(scope) {
      return $(scope).find('.shiny-distr-input');
    },
	getId: function(el) {
      return Shiny.InputBinding.prototype.getId.call(this, el) || el.name;
    },
	getValue: function(el) {
		var params = new Array();
		var id = $(el).attr("id");
		$("#distrInputs"+id).find("input[type=number]").each(function() {
			params.push({name: $(this).attr("name"), value: $(this).val()});
		});
		return({distribution: parseInt($("#select"+id).val())+1, params: params});
	},
	setValue: function(el, value) {
		$(el).attr("value", value);
    },
	subscribe: function(el, callback) {
	  var id = $(el).attr("id");
	  var self = this;
	  $("#select"+id).on("change", function(event) {
		event.stopImmediatePropagation();
		$("#distrInputs"+id).empty();
		var selected = $(this).val();
		var parameters = self.distributions[id][selected].params;
		for(var i=0; i<parameters.length; i++) {
			$("#distrInputs"+id).append("<span style='display:inline-flex;'><label style='display:inline-block;margin-left:15px;'>"+parameters[i].name.capitalize()+ ": </label><input id='distrInput-"+parameters[i].name+ "-" + id + "' name='"+ parameters[i].name + "' value='"+ parameters[i].value +"' style='display:inline-block;width:4.5em;margin-left:5px;' type='number'/></span>");
			$("#distrInput-"+parameters[i].name + "-" + id).on("change", {posparam:i}, function(event) {
        self.distributions[id][selected].params[event.data.posparam].value = $(this).val();
        callback(false);
			});
		}
		callback(false);
	  });
    },
    unsubscribe: function(el) {
      $(el).off('.selectDistrInputBinding');
    },
	receiveMessage: function(el, data) {
		var id = $(el).attr("id");
		if (data.hasOwnProperty('distributions')) {
      this.distributions[id] = data.distributions;
			for (var i=0; i<data.distributions.length; i++) {
				$("#select"+id).append("<option value=" + i + ">" + data.distributions[i].name + "</option>");
			}
		}
    var defval = $(el).attr("defaultvalue");
    if (defval != null) {
      defval = eval("(" + defval + ")");
      this.distributions[id][defval.id-1].params = defval.params;
      $("#select"+id).val(defval.id-1);
    }
		$("#select"+id).trigger('change');
	},
	getState: function(el) {
		return($(el).attr("value"));
    },
	initialize: function(el) {
		this.distributions = new Array();
		var id = $(el).attr("id");
    if (!this.distributions.hasOwnProperty(id)) {
  		$(el).append("<select id='select" + id + "' style='display:inline'></select>").append("<div id='distrInputs" + id + "' style='display:inline-block; height:auto; width:auto;'></div>");
    }
	}
});
Shiny.inputBindings.register(selectDistrInputBinding, 'shiny.selectDistrInputBinding');

//Creamos una especializacion de TextInput para introducir vectores num?ricos
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
	setValue: function(el, value) {
      el.value = value;
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
		  if (data.hasOwnProperty('value') && data.value != null) {
  			res = "";
  			for(var i=0; i<data.value.length; i++) {
  				res += data.value[i] + "; ";
  			}
			  this.setValue(el, res.substr(0, res.length-2));
		  }
     
		  if (data.hasOwnProperty('label') && data.label != null) {
			  $(el).parent().find('label[for=' + el.id + ']').html("<b>" + data.label + "</b>");
		  }
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
        var prevn = parseInt($(this).attr("prev-value"));
		  	var n = $(this).val();
  	    self._changeNodes(id, self, prevn, n);
		});
		
		$(el).on("send.matrixInputBinding", function(event) {
			callback(false);
		});
	},
	receiveMessage: function(el, data) {
		  var id = $(el).attr("id");
      if (data.hasOwnProperty('numNodes')) {
        this._changeNodes(id, this, this.nodes[id], data.numNodes);
				this.nodes[id] = data.numNodes;
	    }
		  if (data.hasOwnProperty('value') && data.hasOwnProperty('size')) {
  			this._changeNodes(id, this, this.nodes[id], data.size);
    		this.nodes[id] = data.size;
        console.log(data.value);
			for(var i=1; i<=data.size; i++)
				for(var j=0; j<data.size; j++) {
					$("#"+id+"-table-"+i+"-"+(j+1)+"-input").attr("value", data.value[j+1][i-1]).trigger("change");
				}
		  }
      $(el).trigger("send.matrixInputBinding");
	},
	initialize: function(el) {	
		if(!this.hasOwnProperty("matrix"))
			this.matrix = new Array();	
	
		var id = $(el).attr("id");		
		$(el).append("<button id='"+ id +"-manualInput' class='btn btn-info'>Edit/View</button>" +
					  "<div id='"+ id +"-manualDialog' style='maxHeight:520; maxWidth:1200'>" +
							"<label for='" + id + "-numNodes'>Number of nodes: </label><input id='"+ id +"-numNodes' type=number value=5 prev-value=5 min=1 max=10/><br><br>" +
							"<center><table id='" + id + "-table'></table></center>" +
					  "</div>");
    if($(el).data("withoutnodes")) {
      $("label[for='" + id + "-numNodes']").first().css("display", "none");
      $("#"+id+"-numNodes").css("display", "none");
    }
    this.nodes = new Array()
		if ($(el).attr("ini-value") != null) {
			inivalue = $(el).attr("ini-value").split("],[");
			this.nodes[id] = inivalue.length;
			this.matrix[id] = new Array(this.nodes[id]);
			for(i=0; i<inivalue.length; i++) {
				rowvalues = inivalue[i].replace("[", "").replace("]","").split(",");
				this.matrix[id][i] = new Array(this.nodes[id]);
				for(j=0; j<rowvalues.length; j++)
					this.matrix[id][i][j] = parseFloat(rowvalues[j]);
			}
			$("#" + id + "-numNodes").val(this.nodes[id]).attr("prev-value", this.nodes[id]);
		}
		else{
		 this.nodes[id] = parseInt($("#" + id + "-numNodes").val());
			if (!this.matrix.hasOwnProperty(id)) {
				this.matrix[id] = new Array(this.nodes[id]);
				for (var i=0; i<this.nodes[id]; i++) {
					this.matrix[id][i] = new Array(this.nodes[id]);
					for (var j=0; j<this.nodes[id]; j++) {
						this.matrix[id][i][j] = 0;
					}
				}	
			}
		}
		this._expandTable(id, 0 , this.nodes[id]);
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
			},
      create: function() {
        $(this).closest('.ui-dialog').find('.ui-button').addClass("btn").addClass("btn-info");
      }
		});
		
		$("#" + id + "-manualInput").button().click(function() {
			$("#" + id + "-manualDialog").dialog("open");
		});
	},
  _changeNodes: function(id, self, prevn, n) {      
  		event.stopImmediatePropagation();
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
				$("#" + id + "-numNodes").attr("prev-value", $("#" + id + "-numNodes").val()); 	
			}
			$("#" + id + "-numNodes").val($("#" + id + "-numNodes").attr("prev-value"));
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
		//Eliminamos las ?ltimas filas
		for(var i=prevn-n+1; i<=prevn; i++) {
		-	$("#" + id + "-table-" + i).remove();
		}
		this.matrix[id].length -= n;
		
		//Eliminamos las celdas de las n pen?ltimas columnas en todas las filas restantes
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

var debounce = function(threshold, func) {
    var timerId = null;
    var self, args;
    return function() {
      self = this;
      args = arguments;
      if (timerId !== null) {
        clearTimeout(timerId);
        timerId = null;
      }
      timerId = setTimeout(function() {
        timerId = null;
        func.apply(self, args);
      }, threshold);
    };
  }
  
var mydatatableOutputBinding = new Shiny.OutputBinding();
  $.extend(mydatatableOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-mydatatable-output');
    },
    onValueError: function(el, err) {
      //Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderError : function(el, err) {
      $(el).empty();
      $(el).append("<p class='shiny-output-error'>"+err.message+"</p>");
    },
    renderValue: function(el, data) {
      var $el = $(el).empty();
      console.log(data);
      if (!data || !data.colnames) return;
      if (data.options.hasOwnProperty("renderHTML") && data.options.renderHTML)
        data.escape = false;
      if(data.options.hasOwnProperty("colnames")) 
        var colnames = $.makeArray(data.options.colnames);
      else 
        var colnames = $.makeArray(data.colnames);
      var header = $.map(colnames, function(x, i) {
        if (i==0 && data.options.hasOwnProperty("rowHeaders") && !data.options.rowHeaders) {
          if (x == "X_empty" || x===undefined)
            return '<th></th>';
        }
        return '<th><b>' + x + '</b></th>';
      }).join('');

      if (data.options.hasOwnProperty("mainTitle")) {
         header = '<thead><tr><th colspan='+ colnames.length  +' class="ui-state-default"><b>'+ data.options.mainTitle +'</b></th></tr><tr>' + header + '</tr></thead>';
      } else{
         header = '<thead><tr>' + header + '</tr></thead>';
      }
      
    var footer = '';
    if(!data.options.hasOwnProperty("noFooter")) {
      if (data.options === null || data.options.searching !== false) {
        footer = $.map(colnames, function(x) {
          // placeholder needs to be escaped (and HTML tags are stripped off)
          return '<th><input type="text" placeholder="' +
                 escapeHTML(x.replace(/(<([^>]+)>)/ig, '')) +
                 '" /></th>';
        }).join('');
        footer = '<tfoot>' + footer + '</tfoot>';
      }
    }
      var content = '<table class="table table-striped table-hover">' +
                    header + footer + '</table>';
      $el.append(content);
      
       // options that should be eval()ed
      if (data.evalOptions)
        $.each(data.evalOptions, function(i, x) {
          /*jshint evil: true */
          data.options[x] = eval('(' + data.options[x] + ')');
        });
      // caseInsensitive searching? default true
      var searchCI = data.options === null || typeof(data.options.search) === 'undefined' ||
                     data.options.search.caseInsensitive !== false;
      var oTable = $(el).children("table").DataTable($.extend({
        "processing": true,
        "serverSide": true,
        "order": [],
        "orderClasses": false,
        "pageLength": 25,
        "ajax": {
          "url": data.action,
          "type": "POST",
          "data": function(d) {
            d.search.caseInsensitive = searchCI;
            d.escape = data.escape;
          }
        },
        "initComplete": function(settings, json) {
                             if (data.options.hasOwnProperty("rowHeaders") && data.options.rowHeaders) {
                               $(el).find("table tbody").find("tr").each(function(){
                                    var td = $(this).find("td").first();
                                    td.addClass("ui-state-default");
                                    td.html("<b>"+td.html()+"</b>");
                               });
                             }
                             Shiny.bindAll($(el));
                          }
        }, data.options));
      
      // the table object may need post-processing
      if (typeof data.callback === 'string') {
        /*jshint evil: true */
        var callback = eval('(' + data.callback + ')');
        if (typeof callback === 'function') callback(oTable);
      }
      
      var searchInputs = $el.find("tfoot input");
      if (searchInputs.length > 0) {
        // this is a little weird: aoColumns/bSearchable are still in DT 1.10
        // https://github.com/DataTables/DataTables/issues/388
        $.each(oTable.settings()[0].aoColumns, function(i, x) {
          // hide the text box if not searchable
          if (!x.bSearchable) searchInputs.eq(i).hide();
        });
        searchInputs.keyup(debounce(data.searchDelay, function() {
          oTable.column(searchInputs.index(this)).search(this.value).draw();
        }));
      }
      // FIXME: ugly scrollbars in tab panels b/c Bootstrap uses 'visible: auto'
      $el.parents('.tab-content').css('overflow', 'visible');
    }
  });
Shiny.outputBindings.register(mydatatableOutputBinding, 'shiny.mydatatableOutput');

var reportBodyOutputBinding = new Shiny.OutputBinding();

$.extend(reportBodyOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-reportbody-output');
    },
    onValueError: function(el, err) {
      //Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderError : function(el, err) {
      $(el).empty();
      $(el).append("<p class='shiny-output-error'>"+err.message+"</p>");
    },
    renderValue: function(el, data) {
      var id = $(el).attr("id");
      var body = "";
      for (i in SessionReports.reports) 
        body += SessionReports.reports[i].body;
      
      $(el).empty().append("<div>"+body+"</div>");
      var dataURI = 'data:text/html,' + encodeURIComponent("<div>"+body+"</div>");
      $("#printAllOutput" + data.numTab).attr("href", dataURI).attr("download", "ArqasReport");
    }
  });
Shiny.outputBindings.register(reportBodyOutputBinding, 'shiny.reportBodyOutputBinding');

var reportMenuOutputBinding = new Shiny.OutputBinding();

$.extend(reportMenuOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-reportmenu-output');
    },
    onValueError: function(el, err) {
      //Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderError : function(el, err) {
      $(el).empty();
      $(el).append("<p class='shiny-output-error'>"+err.message+"</p>");
    },
    renderValue: function(el, data) {
      console.log("Llamada");
      var id = $(el).attr("id");
      var menu = "";
      for (i in SessionReports.reports) 
        menu += SessionReports.reports[i].menu;

      $(el).empty().append("<div><ul>"+menu+"</ul></div>");
      $(el).find("a").each(function(){
          
          var aId = new String($(this).attr("id"));
          if (aId.endsWith("hide")){
            $(this).click(function(){
                var target = $(this).attr("mytarget");
                $("#" + target).toggle();
                var dataURI = 'data:text/html,' + encodeURIComponent($("#ModelOutputBox"+data.numTab).html());
                $("#printAllOutput" + data.numTab).attr("href", dataURI).attr("download", "ArqasReport");
                $(this).next().toggle();
                if ($(this).html() == "[Hide]")
                  $(this).html("[Show]").prev().css('text-decoration', 'line-through');
                else
                  $(this).html("[Hide]").prev().css('text-decoration', '');
              });
          } else if (aId.endsWith("print")) {
              $(this).click(function(){
                  var target = $(this).attr("mytarget");
                  var html = $("#ModelOutputBox" + data.numTab).find("#"+ target).html();
                  var dataURI = 'data:text/html,' + encodeURIComponent(html); 

                  $(this).attr("href", dataURI);
                  $(this).attr("download", $(this).prev().prev().text());
              });
          } else {
            $(this).click(function(){
                var target = $(this).attr("name");
                $("#ModelOutputBox" + data.numTab).scrollTo($("#ModelOutputBox" + data.numTab).find("a[name='"+target+"']").first());
            });
          }
      });
    }
  });
Shiny.outputBindings.register(reportMenuOutputBinding, 'shiny.reportMenuOutputBinding');


var networkOutputBinding = new Shiny.OutputBinding();
$.extend(networkOutputBinding, {
	find: function(scope) {
	  return $(scope).find('.shiny-network-output');
  },
  renderValue: function(el, data) {
		var id = $(el).attr("id");
		if(!this.hasOwnProperty("particleSystem")) {
			this.particleSystem = new Array();
		}
		if (data != null) {
			$(el).empty();
			$(el).append("<canvas id='" + id + "-graphcanvas' width=850 height=540></canvas>");
			this.particleSystem[id] = arbor.ParticleSystem(1000, 50, 0.5, true);
			this.particleSystem[id].renderer = new this._Renderer("#" + id + "-graphcanvas");
      var distrLetter = function(s) {
          if (s.substr(0,1) == "-") return("-");
          if (s.substr(0,3) == "Exp")
            return("M");
          else
            return("G");
      }
			for(i=0;  i<data.s.length; i++) {
        switch(data.type) {
          case "Jackson": {
            this.particleSystem[id].addNode(i+1, {s: data.s[i], nodeText: "M/M/"});
    			  $(el).append("<div id='" + id + "-node-" + (i+1) + "'><b>Node " + (i+1) + ": </b><br>"+
  										      "Lambda: " + data.lambda[i] + "<br>" +
  											    "Mu: " + data.mu[i] + "<br>" +
  											    "S: " + data.s[i] + "<br>" +
  											    "L: " + data.l[i] + "<br>" +
  											    "Lq: " + data.lq[i] + "<br>" +
  											    "W: " + data.w[i] + "<br>" +
  											    "Wq: " + data.wq[i] + "<br></div>");
            break;
          }
          case "Open": {
            var nodetext = distrLetter(data.arrivaldistr[i]) + "/" + distrLetter(data.servicedistr[i]) + "/";
            this.particleSystem[id].addNode(i+1, {s: data.s[i], nodeText: nodetext});
            $(el).append("<div id='" + id + "-node-" + (i+1) + "'><b>Node " + (i+1) + ": </b><br>"+
                            "Arrival Distr.: " + data.arrivaldistr[i] + "<br>" +
                            "Service Distr.: " + data.servicedistr[i] + "<br>" +
    											  "S: " + data.s[i] + "<br>" +
    											  "L: " + data.l[i] + "<br>" +
    											  "Lq: " + data.lq[i] + "<br>" +
    											  "W: " + data.w[i] + "<br>" +
    											  "Wq: " + data.wq[i] + "<br></div>");
            break;
            
          }
          case "Closed": {
            var nodetext = "M/" + distrLetter(data.servicedistr[i]) + "/";
            this.particleSystem[id].addNode(i+1, {s: data.s[i], nodeText: nodetext});
            $(el).append("<div id='" + id + "-node-" + (i+1) + "'><b>Node " + (i+1) + ": </b><br>"+
                            "Service Distr.: " + data.servicedistr[i] + "<br>" +
      										  "S: " + data.s[i] + "<br>" +
    											  "L: " + data.l[i] + "<br>" +
    											  "Lq: " + data.lq[i] + "<br>" +
    											  "W: " + data.w[i] + "<br>" +
    											  "Wq: " + data.wq[i] + "<br></div>");
            break;
          }
        }
          
				$("#" + id + "-node-" + (i+1)).hide().css("position", "absolute").css("background-color", "white").css("border-style", "solid").css("border-width", "2px").css("border-color", "black").css("padding", "2px 2px 2px 2px");
			}
			for(i=0; i<data.s.length; i++) {
        var divProb = "<div id='" + id + "-node-" + (i+1) + "-probs'><b>Probs from Node " + (i+1) + " to: </b><br>";
				for(j=0; j<data.s.length; j++) {
					if(data.p[i][j] != 0) {
              this.particleSystem[id].addEdge((i+1), (j+1), {p: parseFloat(data.p[i][j])});
              divProb += "Node " + (j+1) + ": " + data.p[i][j] + "<br>";
				   }
				}
        divProb += "</div>";
        $(el).append(divProb)
        $("#" + id + "-node-" + (i+1) + "-probs").hide().css("position", "absolute").css("background-color", "white").css("border-style", "solid").css("border-width", "2px").css("border-color", "black").css("padding", "2px 2px 2px 2px");
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
          
          var perp = perpendicularVector(radius, radius+5, 5);
          var endArrow = pointinline({x:pt1.x-2, y:pt1.y+2}, 1, radius, -1);
					var startArrow = pointinline({x:pt1.x-2, y:pt1.y+2}, 1, radius+10, -1);
          ctx.beginPath();
    			ctx.moveTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
					ctx.lineTo(endArrow.x, endArrow.y);
					ctx.lineTo(perp.x+startArrow.x, perp.y+startArrow.y);
          ctx.lineTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
          ctx.fillStyle = "black";
          ctx.fill();
					//var sizeText = ctx.measureText(edge.data.p);
					//ctx.fillText(edge.data.p, pt1.x-sizeText.width/2, pt1.y-2.4*radius);
					
				} else {
					if (parseInt(edge.source.name) > parseInt(edge.target.name)) {
            //Dibujamos la linea
            ctx.beginPath();
            ctx.moveTo(pt1.x, pt1.y);
            ctx.lineTo(pt2.x, pt2.y);
            ctx.stroke();
            var pMedio = {x: ((pt2.x+pt1.x)/2), y: ((pt2.y+pt1.y)/2)};
            //Dibujamos la flecha
            var perp = perpendicularVector((pt2.x-pt1.x), (pt2.y-pt1.y), 5);
            if (pt2.x > pt1.x) {
  						var endArrow = pointinline(pMedio, (pt2.y-pt1.y)/(pt2.x-pt1.x), 15, 1);
							var startArrow = pointinline(pMedio, (pt2.y-pt1.y)/(pt2.x-pt1.x), 5, 1);
						} else { 
							var endArrow = pointinline(pMedio, (pt1.y-pt2.y)/(pt1.x-pt2.x), 15, -1);
							var startArrow = pointinline(pMedio, (pt1.y-pt2.y)/(pt1.x-pt2.x), 5, -1);
						}
            ctx.beginPath();
  					ctx.moveTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
						ctx.lineTo(endArrow.x, endArrow.y);
						ctx.lineTo(perp.x+startArrow.x, perp.y+startArrow.y);
            ctx.lineTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
            ctx.fillStyle = "black";
            ctx.fill();
						/*var perp = perpendicularVector((pt2.x-pt1.x), (pt2.y-pt1.y), 20);
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
						ctx.fillText(edge.data.p, pMedio.x-sizeText.width/2, pMedio.y);*/
						
					} else {
            ctx.beginPath();
  					ctx.moveTo(pt1.x, pt1.y);
						ctx.lineTo(pt2.x, pt2.y);
						ctx.stroke();
            var pMedio = {x: ((pt2.x+pt1.x)/2), y: ((pt2.y+pt1.y)/2)};
            if (pt2.x > pt1.x) {
  						var endArrow = pointinline(pMedio, (pt2.y-pt1.y)/(pt2.x-pt1.x), 15, 1);
							var startArrow = pointinline(pMedio, (pt2.y-pt1.y)/(pt2.x-pt1.x), 5, 1);
						} else { 
							var endArrow = pointinline(pMedio, (pt1.y-pt2.y)/(pt1.x-pt2.x), 15, -1);
							var startArrow = pointinline(pMedio, (pt1.y-pt2.y)/(pt1.x-pt2.x), 5, -1);
						}
            var perp = perpendicularVector((pt2.x-pt1.x), (pt2.y-pt1.y), 5);
            ctx.beginPath();
  					ctx.moveTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
						ctx.lineTo(endArrow.x, endArrow.y);
						ctx.lineTo(perp.x+startArrow.x, perp.y+startArrow.y);
            ctx.lineTo(-perp.x+startArrow.x, -perp.y+startArrow.y);
            ctx.fillStyle = "black";
            ctx.fill();
					  /*  var perp = perpendicularVector((pt2.x-pt1.x), (pt2.y-pt1.y), 20);
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
						ctx.fillText(edge.data.p, pMedio.x-sizeText.width/2, pMedio.y);*/
					}
				}
				
			})

			particleSystem.eachNode(function(node, pt){
			  // node: {mass:#, p:{x,y}, name:"", data:{}}
			  // pt:   {x:#, y:#}  node position in screen coords

			  // draw a circle centered at pt
				ctx.beginPath();
				ctx.arc(pt.x, pt.y, radius, 0, 2 * Math.PI, false);
				ctx.fillStyle = (node.data.p) ? "orange" : "white"
				ctx.fill();
				ctx.fillStyle = "black";
				var sizeText = ctx.measureText(node.data.nodeText+node.data.s);
				ctx.fillText("Node " + node.name, pt.x-ctx.measureText("Node " + node.name).width/2, pt.y-15);
				ctx.fillText(node.data.nodeText + node.data.s, pt.x - sizeText.width/2, pt.y);
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
					
					var id = $(canvas).attr("id").split("-graphcanvas")[0];
					if (!nearest.node) return false;
					if (nearest.distance < 50) {
						$("#" + id + "-node-" + nearest.node.name).css("left", _mouseP.x+50).css("top", _mouseP.y+70).show();
            $("#" + id + "-node-" + nearest.node.name + "-probs").css("left", _mouseP.x+50).css("top", _mouseP.y+70);
					}
					else{
						$("#" + id + "-node-" + nearest.node.name).hide();
            $("#" + id + "-node-" + nearest.node.name + "-probs").hide();
					}
				},
        mouseDown: function(e) {
          var pos = $(canvas).offset();
				  _mouseP = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)
				  var nearest = particleSystem.nearest(_mouseP);
        
          var id = $(canvas).attr("id").split("-graphcanvas")[0];
        
				  if (nearest && nearest.node !== null){
            if ($("#" + id + "-node-" + nearest.node.name + "-probs").is(":hidden")) {
              $("#" + id + "-node-" + nearest.node.name + "-probs").css("left", _mouseP.x+50).css("top", _mouseP.y+70).show();
				    }
            
          }
          $(canvas).mouseup(handler.mouseUp);
        },
        mouseUp: function(e) {
          var pos = $(canvas).offset();
  			  _mouseP = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)
				  var nearest = particleSystem.nearest(_mouseP);
        
          var id = $(canvas).attr("id").split("-graphcanvas")[0];
        
				  if (nearest && nearest.node !== null){
            if ($("#" + id + "-node-" + nearest.node.name + "-probs").is(":visible")) {
              $("#" + id + "-node-" + nearest.node.name + "-probs").hide();
				    }  
          }
          $(canvas).unbind('mouseup', handler.mouseUp);
        },
			  clicked:function(e){
				var pos = $(canvas).offset();
				_mouseP = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)
				dragged = particleSystem.nearest(_mouseP);
        
        var id = $(canvas).attr("id").split("-graphcanvas")[0];
        
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
      $(canvas).mousedown(handler.mouseDown);
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
		$(el).append("<button id='"+ id +"-manualInput'>Edit/View</button>" +
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
			
			var writeMenu = function(data, level) {
				var aux = "";
				for(var i=0; i<data.length; i++) {
          if (data[i].behavior != undefined && data[i].behavior.type == "link") {
            aux += "<li><a href='" + data[i].behavior.link + "' target='_blank'>" + data[i].title + "</a>\n";
                }
          else
            aux += "<li><a idMenu='"+ data[i].id +"' href='#'>" + data[i].title + "</a>\n";
  					if (data[i].submenu.length > 0)
  						aux += "<ul>" + writeMenu(data[i].submenu, level+1) + "</ul>\n";
  					aux += "</li>";
				}
				return aux;
			};
			$el.html(writeMenu(self.menu, 0));
			
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
		$(el).menu({position: {at: "left bottom"}});
	},
	
	_buildMenu: function(data) {
		var menu = new Array();
		for(var i=0; i < data.length; i++) {
			var title = data[i].title;
			if (data[i].submenu.length > 0)
				var submenu = this._buildMenu(data[i].submenu);
			else
				var submenu = new Array();
			menu.push({title: title, submenu: submenu, id: data[i].id, behavior: data[i].behavior});
		}
		return menu;
 	}
  });
  Shiny.inputBindings.register(menuInputBinding, 'shiny.menuInputBinding');
  
   //Creamos un objeto que guarde los valores m?s importantes de un Tab de Jquery UI
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
 
 var clicksTemp= 0;
 
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
        console.log("Agregando pestaña");
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
							
						$ul.find("li:last").each(function() {
							$(this).on("click", function() {
								$el.trigger("shown");
							});
						})
						
						$ul.find("a:last").each(function() {
							$(this).on("click", function(event) {
								$el.trigger("clicktab");
							});
						});
						
						$el.append("<div id='"+ $el.attr("id") +"-tag-"+ tabObject.total + "'>" + data.value[i].content + "</div>\n"); 
            //Si es la plantilla de los modelos actualizamos las alturas
						Shiny.initializeInputs($("#"+$el.attr("id") +"-tag-"+ tabObject.total));
            if ($el.attr("id").indexOf("ModelOutputTabs") > -1 ) {
              $el.css('overflow', 'scroll');
            }
            resizeColumns($("#" + $el.attr("id") + "-tag-" + tabObject.total));
            console.log("Actualizando Pestaña");
						Shiny.unbindAll($el.children("div:last"));
            $el.tabs("refresh");
            $el.tabs("option", "active", self.infoTabs[$el.attr("id")].size - 1);
            Shiny.bindAll($el.children("div:last"));
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
			
	    //Si no est? inicializado el elemento lo hacemos
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
  
$(window).on("resize", function(){
        //resizeColumns($("#results").find("[aria-expanded=true]").first());
});
  
function resizeColumns (scope) {
  var InputDataBoxAlone = $(scope).find(".InputDataBox[alone]").first();
  var InputDataBox = $(scope).find(".InputDataBox").first();
  var Toolbox = $(scope).find(".ToolsBox").first();
  var ModelOutputBox = $(scope).find(".ModelOutputBox").first();
  var ModelOutputTabs = $(scope).find(".ModelOutputTabs").first();
  
  if (InputDataBoxAlone.height() != null) {
    InputDataBoxAlone.css("min-height", InputDataBoxAlone.height()*3);
    ModelOutputTabs.css("min-height", InputDataBoxAlone.height()*0.8);
    $("#results").css("min-height", InputDataBoxAlone.height()*1.1);
  }
  
  if (InputDataBox.height() != null & Toolbox.height() != null) {
      ModelOutputBox.css("min-height", (InputDataBox.height()+Toolbox.height())*1.05);
      $("#results").css("min-height", (InputDataBox.height()+Toolbox.height())*1.15);
      ModelOutputTabs.css("min-height", (InputDataBox.height()+Toolbox.height())*0.9);
  }  
}