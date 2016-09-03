var spec = {"config": {"cell": {"width": 500, "height": 350}}, "mark": "point", "data": {"values": [{"b": 2, "a": "C"}, {"b": 7, "a": "C"}, {"b": 4, "a": "C"}, {"b": 1, "a": "D"}, {"b": 2, "a": "D"}, {"b": 6, "a": "D"}, {"b": 8, "a": "E"}, {"b": 4, "a": "E"}, {"b": 7, "a": "E"}]}};
var selector = "#516dae39-d227-43ac-9730-30e0bfda52ea";
var type = "vega-lite";

var output_area = this;
console.log(spec);
require(['nbextensions/jupyter-vega/index'], function(vega) {
    vega.render(selector, spec, type, output_area);
}, function (err) {
    if (err.requireType !== 'scripterror') {
        throw(err);
    }
});
