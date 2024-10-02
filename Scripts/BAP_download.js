//geometry polygon for site area 
// https://code.earthengine.google.com/60044c59a1020b91ba06da1c2a153b56 link to original script
var dataset = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
    .filterDate('2020-08-01', '2021-01-01');

// Applies scaling factors.
function applyScaleFactors(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0);
  return image.addBands(opticalBands, null, true)
              .addBands(thermalBands, null, true);
}

dataset = dataset.map(applyScaleFactors);

var visualization = {
  bands: ['SR_B4', 'SR_B3', 'SR_B2'],
  min: 0.0,
  max: 0.3,
};


var map = dataset.min().clip(geometry);
var test = map.double().setDefaultProjection(map.projection())
Map.addLayer(map, visualization, 'True Color (432)');

Export.image.toDrive({
  image: test,
  description: "2019BAP",
  scale: 30, 
  
})