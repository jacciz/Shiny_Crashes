function(el, x, data = null) {

  // Built-in Summary Functions
  var summaryFunctions = ["count", "sum", "max", "min", "mean", "median"];

  // Utility function for saving defaults for data parameters
  var param = function(name, defaultValue) { return { name: name, defaultValue : defaultValue}  };

  // Create a list of accepted parameters to this script
  var parameters = [
    param("radius", 12),
    param("opacity", 0.5),
    param("duration", 500),
    param("lowEndColor", 'white'),
    param("highEndColor", 'blue'),
    param("uniformSize", false),
    param("uniformColor", 'blue'),
    param("sizeSummaryFunction", "count"),
    param("colorSummaryFunction", "count"),
    param("sizevar", undefined),
    param("colorvar", undefined)
  ];

  // Function to build the options object for later use
  var buildOptions = function() {

    // Initialize the object that will be filled and returned
    var result = {};

    // Create options object with given values or default values if none are provided
    parameters.forEach(entry => {
      var parameterValue = data[entry.name];
      if(parameterValue !== undefined && parameterValue !== null) {
        result[entry.name] = parameterValue;
      } else {
        result[entry.name] = entry.defaultValue;
      }
    });
    // if no colorvar is set, then mirror with sizevar
    if (result.colorvar === undefined) {
      result.colorvar = result.sizevar;
    }
    // Return options chosen by the user plus defaults for unchosen fields
    return result;
  };
  // Set Options based on parameters given by the data parameters
  var options = buildOptions();


  // Builds a function to calculate per hexagon from the function name
  // or valid js string and variable name to use
  var buildSumFunction = function(functionToUse, variableToUse, variableType) {
    var retFunction;
    var count = dataPointsPerHex => dataPointsPerHex.length;
    // Default to using count
    if(functionToUse == "count" || variableToUse === undefined) return count;
    if(!summaryFunctions.includes(functionToUse)) {
      console.warn("Using Custom Function as no supported functions were matched");
      // User defined function
      retFunction = Function('"use strict";return (' + functionToUse + ')');
    } else {
      retFunction = function(dataPointsPerHex) {
        // Set the inital value for the result
        var result = 0;
        if(functionToUse == "max") result = Number.MIN_VALUE;
        if(functionToUse == "min") result = Number.MAX_VALUE;
        if(functionToUse == "sum") result = 0;

        console.log(dataPointsPerHex.length);
        if(functionToUse == "median") {
          var values = dataPointsPerHex.map(point => point.o[2][variableToUse]);
          if(values.length < 3) {
            if(values.length == 1) result = values[0];
            else if(values.length == 0) result = 0;
            else result = (values[0] + values[1]) / 2;
          } else {
            values.sort((a, b) => a - b);
            var index = (values.length + 1) / 2;
            if(Number.isInteger(index)) {
              result = values[index];
            } else {
              result = (values[Math.floor(index)] + values[Math.ceil(index)]) / 2;
            }
          }

        } else {
          dataPointsPerHex.forEach(point => {
            var value = point.o[2][variableToUse];
            if(functionToUse == "mean") result += value;
            if(functionToUse == "sum") result += value;
            if(functionToUse == "max") result = result > value ? result : value;
            if(functionToUse == "min") result = result < value ? result : value;
          });
          if(functionToUse == "mean") result /= dataPointsPerHex.length;
          if(variableType == "size") result = scaleFunctions[variableToUse](result);
        }
        return result;
      };
      return retFunction;
    }
  };

  // Add Options to hexlayer
  var hexLayer = L.hexbinLayer(options).addTo(this);

  // Add ColorScale
  var colorScale = [options.lowEndColor, options.highEndColor];
  if(data.uniformColor !== null && data.uniformColor !== undefined) {
    colorScale = [data.uniformColor, data.uniformColor];
  }
  hexLayer.colorScale().range(colorScale);

  // Set Radius range based on radius given through data parameter
  var largestRadius = options.radius - 1 / options.radius; // 1/radius is a small buffer between hexagons
  var smallestRadius = options.radius / 2;
  if(options.uniformSize) smallestRadius = largestRadius;
  var radRange = [ smallestRadius, largestRadius ];

  // Add Data to the Hex Layer
  var hexData = [];

  // Check if variable is an array with the same length as the latitute and longitude arrays
  var variableIsCompatible = entry => Array.isArray(entry) && entry.length == data.mapData.lat.length;
  var scaleFunctions = {};
  // Find any variables that may be involved in the hexbin
  var auxData = Object.assign({}, data.mapData);
  Object.keys(auxData).forEach(key => {
    if(key == "lat" || key == "lng") {
      delete auxData[key];
    } else if(variableIsCompatible(auxData[key])) {
      var array = auxData[key];
      var min = Math.min(...array);
      var max = Math.max(...array);
      var radSizeRange = (largestRadius - smallestRadius);
      var variableRange = (max - min);
      scaleFunctions[key] = function(variableValue) {
        return (((variableValue - min) * radSizeRange) / variableRange) + smallestRadius;
      };
    } else {
      delete auxData[key];
    }
  });

  for(i=0; i< data.mapData.lat.length; i++){
    var tuple = [data.mapData.lat[i], data.mapData.lng[i]];
    hexData.push(tuple);
  }
  if(auxData !== undefined) {
    for(i=0; i < hexData.length; i++) {
      var extraVarContainer = {};
      if(options.sizevar !== undefined) {
        extraVarContainer[options.sizevar] = auxData[options.sizevar][i];
      }
      if(options.colorvar !== undefined) {
        extraVarContainer[options.colorvar] = auxData[options.colorvar][i];
      }
      hexData[i].push(extraVarContainer);
    }
  }
  // hexData is now an Array of arrays (Matrix)
  // 0: [lat0, lng0, somevariable0, anothervariable0]
  // 1: [lat1, lng1, somevariable1, anothervariable1]
  // ...

  // Create Hex Layer
  hexLayer
  .radiusRange(radRange)
  .lat(function(d) { return d[0]; })
  .lng(function(d) { return d[1]; })
  .colorValue(buildSumFunction(options.colorSummaryFunction, options.colorvar, "color"))
  .radiusValue(buildSumFunction(options.sizeSummaryFunction, options.sizevar, "size")) // Choose summary function based off of parameter inside the data object
  .data(hexData);


  // Zoom in to fit the data in the screen
  this.fitBounds(hexData);

}