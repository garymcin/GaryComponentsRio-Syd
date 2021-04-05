{***************************************************************************)
{ TMS FMX WebOSMaps component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2013 - 2016                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit GEMWebOSMapsConst;

interface

{$I TMSDEFS.INC}

const
  HTML_BLANK_PAGE       = 'about:blank';
  JAVASCRIPT            = 'JavaScript';

  DEFAULT_ZOOM          = 10;
  DEFAULT_LATITUDE      = 48.85904; // Eiffel Tower Paris Latitude
  DEFAULT_LONGITUDE     = 2.294297;  // Eiffel Tower Paris Longitude
  DEFAULT_WIDTH         = 100;
  DEFAULT_HEIGHT        = 100;

  HTML_FILE_1 = '<!DOCTYPE html>' + #13 +
                '<html>' + #13 +
                '<head>' + #13 +
                '<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no" />' + #13 +
                '<meta http-equiv="content-type" content="text/html; charset=UTF-8" />' + #13 +
                '<meta http-equiv="X-UA-Compatible" content="IE=9">' + #13 +

                '<style type="text/css">' + #13 +
                '  html, body, #Map {' + #13 +
                '  margin: 0;' + #13 +
                '  padding: 0;' + #13 +
                '  height: 100%' + #13 +
                ' }' + #13 +
                '</style>' + #13 +
                '<script>' + #13 +
                '  function SetValues()'+#13+
                '    {'+#13+
                '    mx = window.event.clientX;'+#13+
                '    my = window.event.clientY;'+#13+
                '    }'+#13+
                '</script>' + #13 +
                '</head>' + #13 +
                '<body>' + #13 +

                '<div id="Map" onmousedown=SetValues();></div>' + #13
                ;

  HTML_FILE_2 = '<link rel="stylesheet" href="%CSS%"  type="text/css">' + #13 +
                '<script src="%JS%"></script>' + #13 +
                '%langjs%' + #13 +
                '<script type="text/javascript">' + #13 +
                // tell OpenLayers where the control images are
                'OpenLayers.ImgPath = "%IMG%";' + #13 +
                '%lang%' + #13 +

                'var map;' + #13 +
                'var defaultlat = %latitude%;' + #13 +
                'var defaultlon = %longitude%;' + #13 +
                'var zoom = %zoom%;' + #13 +
                'var allpolygons = [];' + #13 +
                'var allpolylines = [];' + #13 +
                'var allmarkers = [];' + #13 +
                'var allmarkerdrag = [];' + #13 +
                'var selMarker = -1;' + #13 +
                'var startMarkerDrag = false;' + #13 +
                'var dragOffsetLat;' + #13 +
                'var dragOffsetLon;' + #13 +
                'var mx = 0;' + #13 +
                'var my = 0;' + #13 +

                'var fromProjection = new OpenLayers.Projection("EPSG:4326"); // Transform from WGS 1984' + #13 +
                'var toProjection = new OpenLayers.Projection("EPSG:900913"); // to Spherical Mercator Projection' + #13 +
                'var defaultposition = new OpenLayers.LonLat(defaultlon, defaultlat).transform( fromProjection, toProjection);' + #13 +

                //Render map without any controls
                'map = new OpenLayers.Map("Map", {controls: []});' + #13 +
//                'map = new OpenLayers.Map("Map");' + #13 +

                //Add OpenStreetMap layer
                'var mapnik = new OpenLayers.Layer.OSM("Map", '
                + '["http://a.tile.openstreetmap.org/${z}/${x}/${y}.png","http://b.tile.openstreetmap.org/${z}/${x}/${y}.png","http://c.tile.openstreetmap.org/${z}/${x}/${y}.png"], ' +
                '{' + #13 +
                '    "tileOptions": {' + #13 +
                '        "crossOriginKeyword": null' + #13 +
                '  }' + #13 +
                '});' + #13 +

                'mapnik.events.register("loadend", mapnik, function() {' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalMapTilesLoad();' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://tilesload"); '+ #13 +
                {$ENDIF}
                '});' + #13 +

                'map.addLayer(mapnik);' + #13 +

                // Add Map controls

                //default map controls
                'map.addControl(new OpenLayers.Control.Navigation());' + #13 +
                'map.addControl(new OpenLayers.Control.ArgParser());' + #13 +
                'map.addControl(new OpenLayers.Control.Attribution());' + #13 +

                'layerswitch = new OpenLayers.Control.LayerSwitcher();' + #13 +
//                'if (%layerswitch%) {' + #13 +
                'map.addControl(layerswitch);' + #13 +
                'layerswitch.div.style.display = "%layerswitchDisplay%";' + #13 +
                'var top = %layerswitchTop%;' + #13 +
                'var left = %layerswitchLeft%;' + #13 +
                'if ((top != -1) && (left != -1)) {' + #13 +
                '  layerswitch.div.style.top = top + "px";' + #13 +
                '  layerswitch.div.style.left = left + "px";' + #13 +
                '}' + #13 + #13 +

                'overviewmap = new OpenLayers.Control.OverviewMap();' + #13 +
//                'if (%overviewMapControl%) {' + #13 +
                'map.addControl(overviewmap);' + #13 +
                'overviewmap.div.style.display = "%overviewmapDisplay%";' + #13 +

                'scaleline = new OpenLayers.Control.ScaleLine();' + #13 +
//                'if (%scaleControl%) {' + #13 +
                'map.addControl(scaleline);' + #13 +
                'scaleline.div.style.display = "%scaleControlDisplay%";' + #13 +
                'var top = %scaleControlTop%;' + #13 +
                'var left = %scaleControlLeft%;' + #13 +
                'if ((top != -1) && (left != -1)) {' + #13 +
                '  scaleline.div.style.top = top + "px";' + #13 +
                '  scaleline.div.style.left = left + "px";' + #13 +
                '}' + #13 +

                'panzoombar = new OpenLayers.Control.PanZoomBar();' + #13 +
//                'if (%panControl%) {' + #13 +
                'map.addControl(panzoombar);' + #13 +
                'panzoombar.div.style.display = "%panzoombarDisplay%";' + #13 +
                'var top = %panzoombarTop%;' + #13 +
                'var left = %panzoombarLeft%;' + #13 +
                'if ((top != -1) && (left != -1)) {' + #13 +
                '  panzoombar.div.style.top = top + "px";' + #13 +
                '  panzoombar.div.style.left = left + "px";' + #13 +
                '}' + #13 +

//                '	mouseposition = new OpenLayers.Control.MousePosition();' + #13 +
                'mouseposition = new OpenLayers.Control.MousePosition( {id: "mouseposition", prefix: "%mpprefix% ", displayProjection: fromProjection, numDigits: 2} );' + #13 +
                'map.addControl(mouseposition);' + #13 +
                'mouseposition.div.style.display = "%mousepositionDisplay%";' + #13 +
                'var top = %mousepositionTop%;' + #13 +
                'var left = %mousepositionLeft%;' + #13 +
                'if ((top != -1) && (left != -1)) {' + #13 +
                '  mouseposition.div.style.top = top + "px";' + #13 +
                '  mouseposition.div.style.left = left + "px";' + #13 +
                '}' + #13 +

                'keyboardefaults = new OpenLayers.Control.KeyboardDefaults();' + #13 +
                'if (%keyboardShortcuts%)' + #13 +
                ' 	map.addControl(keyboardefaults);' + #13 +

                //disable map dragging
                'if (!%draggable%) {' + #13 +
                'for (var i = 0; i< map.controls.length; i++) {' + #13 +
                '  if (map.controls[i].displayClass ==' + #13 +
                '                          "olControlNavigation") {' + #13 +
                '    map.controls[i].deactivate();' + #13 +
                '  }' + #13 +
                '}' + #13 +
                '}' + #13 +

                //disable double click zoom
                'if (%disableDoubleClickZoom%) {' + #13 +
                '  var Navigation = new OpenLayers.Control.Navigation({' + #13 +
                '    defaultDblClick: function(event) { return; }' + #13 +
                '  });' + #13 +
                '  map.addControl(Navigation);' + #13 +
                '}' + #13 +

                //disable scroll wheel zoom
                'if (!%scrollwheel%) {' + #13 +
                '  controls = map.getControlsByClass("OpenLayers.Control.Navigation");' + #13 +
                '  for(var i = 0; i < controls.length; ++i)' + #13 +
                '    controls[i].disableZoomWheel();' + #13 +
                '}' + #13 +

                //init Map
                'map.setCenter(defaultposition, zoom);' + #13 +

                //init vectors layer
                'var vectors = new OpenLayers.Layer.Vector("Polygons");' + #13 +
                'var ipol = -1;' + #13 +
                'map.addLayer(vectors);' + #13 +

                //init vectors layer drag & drop
                'drag = new OpenLayers.Control.DragFeature(vectors, {' + #13 +
                '    autoActivate: true,' + #13 +
                '    onStart: function(feature) {' + #13 +
                '      id = getObjectId(allpolygons, feature);' + #13 +
                '      if (id > -1) {' + #13 +
                '        ipol = id;' + #13 +
                {$IFNDEF FMXLIB}
                '        external.ExternalPolygonDragStart(id);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '        sendObjectMessage("jsevent://polygondragstart:id="+id); '+ #13 +
                {$ENDIF}
                '      }' + #13 +
                '      id = getObjectId(allpolylines, feature);' + #13 +
                '      if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '        external.ExternalPolylineDragStart(id);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '        sendObjectMessage("jsevent://polygondragstart:id="+id); '+ #13 +
                {$ENDIF}
                '    },' + #13 +
                '    onDrag: function(feature) {' + #13 +
                '      lonlatbounds = feature.geometry;' + #13 +
                '      lonlatbounds = lonlatbounds.transform(toProjection, fromProjection);' + #13 +
                '      lonlat = lonlatbounds.getVertices();' + #13 +
                '      id = getObjectId(allpolygons, feature);' + #13 +
                '      if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '        external.ExternalPolygonDrag(id,String(lonlat).replace(/\)/g,"").replace(/POINT\(/g,"").replace(/ /g,"^"));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '        sendObjectMessage("jsevent://polygondrag:id="+id+"#lonlat="+String(lonlat).replace(/\)/g,"").replace(/POINT\(/g,"").replace(/ /g,"^")); '+ #13 +
                {$ENDIF}
                '      id = getObjectId(allpolylines, feature);' + #13 +
                '      if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '        external.ExternalPolylineDrag(id,String(lonlat).replace(/\)/g,"").replace(/POINT\(/g,"").replace(/ /g,"^"));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '        sendObjectMessage("jsevent://polylinedrag:id="+id+"#lonlat="+String(lonlat).replace(/\)/g,"").replace(/POINT\(/g,"").replace(/ /g,"^")); '+ #13 +
                {$ENDIF}
                //reset transformed geometry values to avoid issues with next drag operation
                '      lonlatbounds = lonlatbounds.transform(fromProjection, toProjection);' + #13 +
                '    },' + #13 +
                '    onComplete: function(feature) {' + #13 +
                '      lonlatbounds = feature.geometry;' + #13 +
                '      lonlatbounds = lonlatbounds.transform(toProjection, fromProjection);' + #13 +
                '      lonlat = lonlatbounds.getVertices();' + #13 +
                '      id = getObjectId(allpolygons, feature);' + #13 +
                '      if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '        external.ExternalPolygonDragEnd(id,String(lonlat).replace(/\)/g,"").replace(/POINT\(/g,"").replace(/ /g,"^"));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '        sendObjectMessage("jsevent://polygondragend:id="+id+"#lonlat="+String(lonlat).replace(/\)/g,"").replace(/POINT\(/g,"").replace(/ /g,"^")); '+ #13 +
                {$ENDIF}
                '      id = getObjectId(allpolylines, feature);' + #13 +
                '      if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '        external.ExternalPolylineDragEnd(id,String(lonlat).replace(/\)/g,"").replace(/POINT\(/g,"").replace(/ /g,"^"));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '        sendObjectMessage("jsevent://polylinedragend:id="+id+"#lonlat="+String(lonlat).replace(/\)/g,"").replace(/POINT\(/g,"").replace(/ /g,"^")); '+ #13 +
                {$ENDIF}
                //reset transformed geometry values to avoid issues with next drag operation
                '      lonlatbounds = lonlatbounds.transform(fromProjection, toProjection);' + #13 +
                //fix make sure drag action is deactivated if DragEnd Allow was set to false
                '      this.handlers.drag.deactivate(); ' + #13 +
                '    }' + #13 +
                '  });' + #13 +
                'map.addControl(drag);' + #13 +

                //vectors hover
                'hoverControl = new OpenLayers.Control.SelectFeature('+ #13 +
                '  [vectors],'+ #13 +
                '  {'+ #13 +
                '    hover: true,'+ #13 +
                '    highlightOnly: true,'+ #13 +
                '    renderIntent: "temporary",'+ #13 +
                '      eventListeners: {'+ #13 +
                '        featurehighlighted: function(e){' + #13 +
                '        id = getObjectId(allpolygons, e.feature);' + #13 +
                '        if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '          external.ExternalPolygonMouseEnter(id);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '          sendObjectMessage("jsevent://polygonmouseenter:id="+id); '+ #13 +
                {$ENDIF}
                '        id = getObjectId(allpolylines, e.feature);' + #13 +
                '        if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '          external.ExternalPolylineMouseEnter(id);},' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '          sendObjectMessage("jsevent://polylinemouseenter:id="+id);},'+ #13 +
                {$ENDIF}
                '          featureunhighlighted: function(e){' + #13 +
                '        id = getObjectId(allpolygons, e.feature);' + #13 +
                '        if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '          external.ExternalPolygonMouseExit(id);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '          sendObjectMessage("jsevent://polygonmousexit:id="+id); '+ #13 +
                {$ENDIF}
                '        id = getObjectId(allpolylines, e.feature);' + #13 +
                '        if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '          external.ExternalPolylineMouseExit(id);}' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '          sendObjectMessage("jsevent://polylinemousexit:id="+id);}'+ #13 +
                {$ENDIF}
                '    }'+ #13 +
                '  }'+ #13 +
                ');'+ #13 +
                'map.addControl(hoverControl);' + #13 +
                'hoverControl.activate();' + #13 +

                //vectors select
                'selectControl = new OpenLayers.Control.SelectFeature('+ #13 +
                '  [vectors],'+ #13 +
                '  {'+ #13 +
                '    clickout: true, toggle: false,'+ #13 +
                '    multiple: false, hover: false'+ #13 +
                '  }'+ #13 +
                ');'+ #13 +
                'map.addControl(selectControl);' + #13 +
                'selectControl.activate();' + #13 +

                //vectors events
                'vectors.events.on({ '+ #13 +
                '  "featureselected": function(e) { '+ #13 +
                //unselect
                '    selectControl.unselect(e.feature);' + #13 +
                //'    selectControl.unselectAll();' + #13 +
                //click
                '    id = getObjectId(allpolygons, e.feature);' + #13 +
                '    if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '      external.ExternalPolygonClick(id);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '      sendObjectMessage("jsevent://polygonclick:id="+id); '+ #13 +
                {$ENDIF}
                '    id = getObjectId(allpolylines, e.feature);' + #13 +
                '    if (id > -1)' + #13 +
                {$IFNDEF FMXLIB}
                '      external.ExternalPolylineClick(id);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '      sendObjectMessage("jsevent://polylineclick:id="+id); '+ #13 +
                {$ENDIF}
                '  } '+ #13 +
//                '    ,"featureunselected": function(e) { '+ #13 +
//                '    } '+ #13 +
                '}); '+ #13 +

                'function getBounds() {' + #13 +
                '  var bounds = map.getExtent();' + #13 +
                '  var ne = new OpenLayers.LonLat(bounds.right, bounds.top).transform(toProjection, fromProjection);' + #13 +
                '  var sw = new OpenLayers.LonLat(bounds.left, bounds.bottom).transform(toProjection, fromProjection);' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalBoundsRetrieved(ne.lat, ne.lon, sw.lat, sw.lon);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://boundsretrieved:nelat="+ne.lat+"#nelng="+ne.lon+"#swlat="+sw.lat+"#swlng="+sw.lon); '+ #13 + //event
                {$ENDIF}
                ' }' + #13 +

                'function setBounds(neLon, neLat, swLon, swLat) {' + #13 +
                '  var bbox = new OpenLayers.Bounds(swLon, swLat, neLon, neLat).transform(fromProjection, toProjection);' + #13 +
                '  map.zoomToExtent(bbox);' + #13 +
                '}' + #13 +

                'function mapPanTo(lon, lat) {' + #13 +
                '  map.panTo(new OpenLayers.LonLat(lon, lat).transform(fromProjection, toProjection));' + #13 +
                '}' + #13 +

                'function createMapPolygon'+
                '(index, clickable, editable, paths, bgcolor, bordercolor, borderwidth, bgopacity, borderopacity, visible, geodesic, zindex, ptype, centerlat, centerlng, radius, nelat, nelng, swlat, swlng, lbText, lbColor, lbFont, lbBold,lbSize, lbOffsetX, lbOffsetY) {' + #13 +
                '  itemcount = allpolygons.length' + #13 +
                '  if (ptype == "line")' + #13 +
                '    itemcount = allpolylines.length;' + #13 +

                'if (index<itemcount) {' + #13 +
                '  mpx = new OpenLayers.Pixel(0, 0);' + #13 +
                '  mpx2 = new OpenLayers.Pixel(0, 0);' + #13 +
                '  newrad = 0;' + #13 +

                'if (ptype == "circle") {' + #13 +
                //convert distance to degrees
                '  crad = (radius / (40075160 / 360));' + #13 +
                '  mpx = map.getLayerPxFromLonLat(new OpenLayers.LonLat(centerlng - crad,centerlat).transform(fromProjection, toProjection));' + #13 +
                '  mpx2 = map.getLayerPxFromLonLat(new OpenLayers.LonLat(centerlng,centerlat).transform(fromProjection, toProjection));' + #13 +
                '  newrad = (mpx2.x - mpx.x);' + #13 +
                '  if (newrad < 0) newrad = newrad * -1;' + #13 +
                '}' + #13 +

                'var style = {' + #13 +
                '   label: lbText,' +
                '   labelXOffset: lbOffsetX,' +
                '   labelYOffset: lbOffsetY,' +
                '   fontColor: "" + lbColor + "",' +
                '   fontSize: "" + lbSize + "px",' +
                '   fontFamily: "" + lbFont + "",' +
                '   fontWeight: "" + lbBold + "",' +
                '   strokeColor: "" + bordercolor + "",' +
                '   strokeOpacity: borderopacity,' +
                '   strokeWidth: borderwidth,' +
                '   fillColor: "" + bgcolor + "",' +
                '   fillOpacity: bgopacity,' +
                '   pointRadius: newrad,' +
                '   display: visible' +
                '};' + #13 +

                'if (ptype == "circle") {' + #13 +
                '  var center = new OpenLayers.LonLat(centerlng,centerlat).transform(fromProjection, toProjection);' + #13 +
                '  polygon = new OpenLayers.Geometry.Point(center.lon,center.lat);' + #13 +
                '}' + #13 +
                'else' + #13 +
                '{' + #13 +
                '  var points = paths;' + #13 +

                '  if (ptype == "line") {' + #13 +
                //Polyline
                '    polygon = new OpenLayers.Geometry.LineString(points);' + #13 +
                '  }' + #13 +
                '  else' + #13 +
                '  {' + #13 +
                //Polygon
                '    var ring = new OpenLayers.Geometry.LinearRing(points);' + #13 +
                '    polygon = new OpenLayers.Geometry.Polygon([ring]);' + #13 +
                '  }' + #13 +
                '}' + #13 +

                // create some attributes for the feature
                //'var attributes = {name: "my name", bar: "foo"};' + #13 +
                //'var feature = new OpenLayers.Feature.Vector(polygon, attributes, style)' + #13 +
                'var feature = new OpenLayers.Feature.Vector(polygon, null, style)' + #13 +

                'if (index>-1) {' + #13 +
                '  if (ptype == "line") {' + #13 +
                '    polygon = allpolylines[index];' + #13 +
                '    vectors.removeFeatures(polygon);' + #13 +
                '    allpolylines[index] = feature;' + #13 +
                '  }' + #13 +
                '  else' + #13 +
                '  {' + #13 +
//                ' alert(index); ' + #13 +
                '    polygon = allpolygons[index];' + #13 +
                '    vectors.removeFeatures(polygon);' + #13 +
                '    allpolygons[index] = feature;' + #13 +
                '  }' + #13 +
                '}' + #13 +
                'else' + #13 +
                '{' + #13 +
                '  if (ptype == "line")' + #13 +
                '    allpolylines.push(feature);' + #13 +
                '  else' + #13 +
                '    allpolygons.push(feature);' + #13 +
                '  }' + #13 +
                '  vectors.addFeatures([feature]);' + #13 +
                '  }' + #13 +
                '}' + #13 +

                'function deleteAllMapPolygon() {' + #13 +
//                ' vectors.removeAllFeatures();' + #13 +
//remove only polygons and not polylines
                '  for (i = 0; i < allpolygons.length; i++)' + #13 +
                '    vectors.removeFeatures(allpolygons[i]);' + #13 +
                '  allpolygons.splice(0,allpolygons.length);' + #13 +
                '}' + #13 +

                'function deleteMapPolygon(Index) {' + #13 +
                '  vectors.removeFeatures(allpolygons[Index]);' + #13 +
                '  allpolygons.splice(Index,1);' + #13 +
                '}' + #13 +

                'function deleteAllMapPolyline() {' + #13 +
                '  for (i = 0; i < allpolylines.length; i++)' + #13 +
                '    vectors.removeFeatures(allpolylines[i]);' + #13 +
                '  allpolylines.splice(0,allpolylines.length);' + #13 +
                '}' + #13 +

                'function deleteMapPolyline(Index) {' + #13 +
                '  vectors.removeFeatures(allpolylines[Index]);' + #13 +
                '  allpolylines.splice(Index,1);' + #13 +
                '}' + #13 +

                'function deleteAllMapMarker() {' + #13 +
                '  markers.clearMarkers();' + #13 +
                '  allmarkers.splice(0,allmarkers.length);' + #13 +
                '}' + #13 +

                'function deleteMapMarker(Index) {' + #13 +
                '  if (Index<allmarkers.length) {' + #13 +
                '    markers.removeMarker(allmarkers[Index]);' + #13 +
                '    allmarkers.splice(Index,1);' + #13 +
                '  } else { ' + #13 +
                {$IFNDEF FMXLIB}
                '    external.ExternalOSMapsError(3);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '    sendObjectMessage("jsevent://error:errorid=3");' + #13 +
                {$ENDIF}
                '  }' + #13 +
                '}' + #13 +

                'function getObjectId(array, object){' + #13 +
                '  IdObject=-1;' + #13 +
                '  for (var i = 0; i < array.length; i++){' + #13 +
                '    if (array[i]==object) {' + #13 +
                '      IdObject=i;' + #13 +
                '    }' + #13 +
                '  }' + #13 +
                '  return IdObject;' + #13 +
                '}' + #13 +

                //init markers layer
                'var markers = new OpenLayers.Layer.Markers( "Markers" );' + #13 +
                'map.addLayer(markers);' + #13 +

                //make marker
                'function makeMarker(index, lon, lat, icon, drag, vis, width, height) {' + #13 +
                '  position = new OpenLayers.LonLat(lon, lat).transform( fromProjection, toProjection);' + #13 +

                '  if (icon != "") {' + #13 +
                '    var size = null;' + #13 +
                '    var offset = null;' + #13 +
                '    size = new OpenLayers.Size(width,height);' + #13 +
                '    offset = new OpenLayers.Pixel(-(size.w/2), -size.h);' + #13 +
//              '  offset = new OpenLayers.Pixel(-(size.w), -size.h);' + #13 +
                '    var iconobj = new OpenLayers.Icon(icon, size, offset);' + #13 +
                '    marker = new OpenLayers.Marker(position,iconobj);' + #13 +
                '  }' + #13 +
                '  else ' + #13 +
                '    marker = new OpenLayers.Marker(position);' + #13 +

                '  if (index > -1) {' + #13 +
                '    exmarker = allmarkers[index];' + #13 +
                '    if (exmarker) {' + #13 +
                '     markers.removeMarker(exmarker);' + #13 +
                '     allmarkers[index] = marker;' + #13 +
                '     allmarkerdrag[index] = drag;' + #13 +
                '     markers.addMarker(marker);' + #13 +
                '    }' + #13 +
                '  }' + #13 +
                '  else' + #13 +
                '  {' + #13 +
                '    allmarkers.push(marker);' + #13 +
                '    allmarkerdrag.push(drag);' + #13 +
                '  markers.addMarker(marker);' + #13 +
                '  }' + #13 +

                '  if (!vis) marker.display(vis);' + #13 +

                //marker events
                '  marker.events.register("click", map, function(e){' + #13 +
                {$IFNDEF FMXLIB}
                '    external.ExternalMarkerClick(getObjectId(allmarkers, e.object));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '    sendObjectMessage("jsevent://markerclick:id="+getObjectId(allmarkers, e.object)); '+ #13 +
                {$ENDIF}
                '  });' + #13 +

                '  marker.events.register("mousedown", map, function(e){' + #13 +
                '    selMarker = getObjectId(allmarkers, e.object);' + #13 +
                '    startMarkerDrag = true;' + #13 +
                {$IFNDEF FMXLIB}
                '    external.ExternalMarkerMouseDown(getObjectId(allmarkers, e.object));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '    sendObjectMessage("jsevent://markermousedown:id="+getObjectId(allmarkers, e.object)); '+ #13 +
                {$ENDIF}
                '  });' + #13 +

                '  marker.events.register("mouseup", map, function(e){' + #13 +
                '    selMarker = -1;' + #13 +
                {$IFNDEF FMXLIB}
                '    external.ExternalMarkerMouseUp(getObjectId(allmarkers, e.object));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '    sendObjectMessage("jsevent://markermouseup:id="+getObjectId(allmarkers, e.object)); '+ #13 +
                {$ENDIF}
                '  });' + #13 +

                '  marker.events.register("mouseout", map, function(e){' + #13 +
                {$IFNDEF FMXLIB}
                '    external.ExternalMarkerMouseExit(getObjectId(allmarkers, e.object));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '    sendObjectMessage("jsevent://markermouseexit:id="+getObjectId(allmarkers, e.object)); '+ #13 +
                {$ENDIF}
                '  });' + #13 +

                '  marker.events.register("mouseover", map, function(e){' + #13 +
                {$IFNDEF FMXLIB}
                '    external.ExternalMarkerMouseEnter(getObjectId(allmarkers, e.object));' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '    sendObjectMessage("jsevent://markermouseenter:id="+getObjectId(allmarkers, e.object)); '+ #13 +
                {$ENDIF}
                '  });' + #13 +
                '}' + #13 +

                //create marker
                'function createMapMarker(index, lon, lat, icon, drag, vis) {' + #13 +

                '  if (index < allmarkers.length) {' + #13 +
                '    if (icon != "") {' + #13 +
                '      iconindex = allmarkers.length;'+
                '      if (index > -1)' +
                '        iconindex = index;' +
                '      var myImage = new Image();' + #13 +
                '      myImage.name = icon;' + #13 +
                '      myImage.onload = function() {' + #13 +
                '        makeMarker(this.index, lon, lat, this.src, drag, vis, this.width, this.height);' + #13 +
                '      }' + #13 +
                '      myImage.src = icon;' + #13 +
//                '      myImage.lon = lon;' + #13 +
//                '      myImage.lat = lat;' + #13 +
//                '      myImage.drag = drag;' + #13 +
//                '      myImage.vis = vis;' + #13 +
                '      myImage.index = iconindex;' + #13 +
                '      makeMarker(index, lon, lat, icon, drag, vis, 21, 25);' + #13 +
                '    }' + #13 +
                '    else ' + #13 +
                '      makeMarker(index, lon, lat, icon, drag, vis, 21, 25);' + #13 +
                '  }' + #13 +
                '}' + #13 +

                //move marker
                'function moveMapMarker(index, lat, lon){' + #13 +
                '  if (index < allmarkers.length) {' + #13 +
                '    ll = map.getLayerPxFromLonLat(new OpenLayers.LonLat(lon,lat).transform(fromProjection, toProjection));' + #13 +
                '    allmarkers[index].moveTo(ll);' + #13 +
                '  }' + #13 +
                '}' + #13 +

                //map events
                'map.events.register("click", map, function(e){' + #13 +
                '  var opx = map.getLonLatFromPixel(e.xy).transform(toProjection, fromProjection);' + #13 +
                '  if (selMarker >= 0) {' + #13 +
                '  if (allmarkerdrag[selMarker])' + #13 +
                {$IFNDEF FMXLIB}
                '    external.ExternalMarkerDragEnd(selMarker,opx.lat,opx.lon);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '    sendObjectMessage("jsevent://markerdragend:id=" + selMarker + "#lat="+opx.lat+"#lng="+opx.lon); '+ #13 +
                {$ENDIF}
                '  selMarker = -1;' + #13 +
                '  }' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalMapClick(opx.lat,opx.lon);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://click:lat="+opx.lat+"#lng="+opx.lon+"#x="+mx+"#y="+my); '+ #13 +
                {$ENDIF}
                '  });' + #13 +

                'map.events.register("mouseover", map, function(e){' + #13 +
                '  var opx = map.getLonLatFromPixel(e.xy).transform(toProjection, fromProjection);' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalMapMouseEnter(opx.lat,opx.lon);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://mouseenter:id=" + selMarker + "#lat="+opx.lat+"#lng="+opx.lon); '+ #13 +
                {$ENDIF}
                '});' + #13 +

                'map.events.register("mouseout", map, function(e){' + #13 +
                '  var opx = map.getLonLatFromPixel(e.xy).transform(toProjection, fromProjection);' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalMapMouseExit(opx.lat,opx.lon);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://mousexit:id=" + selMarker + "#lat="+opx.lat+"#lng="+opx.lon); '+ #13 +
                {$ENDIF}
                '});' + #13 +

                'map.events.register("mouseup", map, function(e){' + #13 +
//                '  selMarker = -1;' + #13 +
                '        ipol = -1;' + #13 +
                '});' + #13 +

//                'map.events.register("mousedown", map, function(e){' + #13 +
//                '  selMarker = -1;' + #13 +
//                '});' + #13 +

                'map.events.register("move", map, function(e){' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalMapMove();' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://drag"); '+ #13 +
                {$ENDIF}
                '});' + #13 +

                'map.events.register("mousemove", map, function(e){' + #13 +
                '  var opx = map.getLonLatFromViewPortPx(e.xy).transform(toProjection, fromProjection);' + #13 +
                '  if (selMarker >= 0) {' + #13 +
                '    if (allmarkerdrag[selMarker]){' + #13 +
//                '      var ex = e.xy.x - (76/4);' +
                '      var ex = e.xy.x;' +
                '      var ey = e.xy.y;' +
                '      var offpx = map.getLonLatFromViewPortPx(new OpenLayers.Pixel(ex,ey)).transform(toProjection, fromProjection);' + #13 +
                '      if (startMarkerDrag) {' + #13 +
                {$IFNDEF FMXLIB}
                '        external.ExternalMarkerDragStart(selMarker,offpx.lat,offpx.lon);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '    sendObjectMessage("jsevent://markerdragstart:id=" + selMarker + "#lat="+opx.lat+"#lng="+opx.lon); '+ #13 +
                {$ENDIF}
                '        startMarkerDrag = false;' + #13 +
//                '        dragOffsetLat = allmarkers[selMarker].lonlat.transform(toProjection, fromProjection).lat - opx.lat;' + #13 +
//                '        dragOffsetLon = allmarkers[selMarker].lonlat.transform(toProjection, fromProjection).lon - opx.lon;' + #13 +
                '      }' + #13 +
//                '      moveMapMarker(selMarker, opx.lat + dragOffsetLat, opx.lon);' + #13 +
                '      moveMapMarker(selMarker, opx.lat, opx.lon);' + #13 +
                {$IFNDEF FMXLIB}
                '      external.ExternalMarkerDrag(selMarker,opx.lat,opx.lon);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '      sendObjectMessage("jsevent://markerdragend:id=" + selMarker + "#lat="+opx.lat+"#lng="+opx.lon); '+ #13 +
                {$ENDIF}
                '    }' + #13 +
                '  }' + #13 +
                {$IFNDEF FMXLIB}
                'external.ExternalMapMouseMove(opx.lat,opx.lon);' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                'sendObjectMessage("jsevent://mousemove:lat="+opx.lat+"#lng="+opx.lon+"#x="+mx+"#y="+my); '+ #13 +
                {$ENDIF}
                '});' + #13 +

                'map.events.register("movestart", map, function(e){' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalMapMoveStart();' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://dragstart"); '+ #13 +
                {$ENDIF}
                '});' + #13 +

                'map.events.register("moveend", map, function(e){' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalMapMoveEnd();' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://dragend"); '+ #13 +
                {$ENDIF}
                '});' + #13 +

                'map.events.register("zoomend", map, function(e){' + #13 +
                {$IFNDEF FMXLIB}
                '  external.ExternalMapZoomChange(map.getZoom());' + #13 +
                {$ENDIF}
                {$IFDEF FMXLIB}
                '  sendObjectMessage("jsevent://zoomchange:zoomlevel="+map.getZoom()); '+ #13 +
                {$ENDIF}
                '});' + #13 +

                {$IFDEF FMXLIB}
                'var sendObjectMessage = function(parameters) {'+#13+
                {$IFDEF ANDROID}
                '  injectedObject.setPrivateImeOptions(parameters); '+ #13 +
                '  injectedObject.performClick();'+ #13 +
                {$ELSE}
                'var iframe = document.createElement(''iframe'');'+#13+
                'iframe.setAttribute(''src'', parameters);'+#13+
                'document.documentElement.appendChild(iframe);'+#13+
                'iframe.parentNode.removeChild(iframe);'+#13+
                'iframe = null;'+#13+
                {$ENDIF}
                ' };'+#13+
                {$ENDIF}

                'function touchStart(event) {'+#13+
                '  var allTouches = event.touches;'+#13+
                '  for (var i = 0; i < allTouches.length; i++){' + #13 +
                '    mx = event.touches[i].pageX;'+#13+
                '    my = event.touches[i].pageY;'+#13+
                '  }'+#13+
                '}'+#13+

                '</script>' + #13 +

                '</body>' + #13 +
                '</html>';

implementation

end.
