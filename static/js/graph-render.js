function newGraph (selector, width, height) {
    var edges = []
    var vertices = {}
    var svg = null;

    function plotGraph (svg, vertices, edges) {
	svg.html("")

	var force = cola.d3adaptor()
	    .nodes(d3.values(vertices))
	    .links(edges)
	    .size([width, height])
	    .linkDistance(60)
	    .on("tick", tick)
	    .start();

	var path = svg.append("g").selectAll("line")
	    .data(force.links())
	    .enter().append("line")
	    .attr("class", "edge")
	    .style("stroke-width", 1)
	    .style("stroke", 'black');

	var circle = svg.append("g").selectAll("circle")
	    .data(force.nodes())
	    .enter().append("circle")
	    .attr("r", 6)
	    .call(force.drag);

	var text = svg.append("g").selectAll("text")
	    .data(force.nodes())
	    .enter().append("text")
	    .attr("x", 8)
	    .attr("y", ".31em")
	    .text(function(d) {
		return d.name.substring(0, 5) + "...";
	    });

	// Use elliptical arc path segments to doubly-encode directionality.
	function tick() {
	    path.attr("x1", function (d) { return d.source.x; })
		.attr("y1", function (d) { return d.source.y; })
		.attr("x2", function (d) { return d.target.x; })
		.attr("y2", function (d) { return d.target.y; });
	    circle.attr("transform", transform);
	    text.attr("transform", transform);
	}

	function transform(d) {
	    return "translate(" + d.x + "," + d.y + ")";
	}
    }

    svg = d3.select(selector).append("svg")
	.attr("width", width)
	.attr("height", height);

    // refresh :: () -> IO ()
    function refresh() {
	plotGraph(svg, vertices, edges)
    }

    // reset :: () -> IO ()
    function reset() {
	vertices = {};
	edges = [];
	refresh();
    }

    // addNode :: String -> IO ()
    function addNode(nodeName) {
	vertices[nodeName] = {name: nodeName}
    }

    // addEdge :: String -> String -> String? -> IO ()
    function addEdge(from, to, label) {
	if (vertices[from] && vertices[to]) {
	    link = {source: vertices[from], target: vertices[to], type: (label || 'suit')}
	    // For some reason, edges.push(link) results in some numeric properties being
	    // converted to NaN. No idea why.
	    edges = edges.concat([link])
	} else if (vertices[from]) {
	    console.error("NO SUCH NODE", to)
	} else {
	    console.error("NO SUCH NODE", from)
	}
    }

    // addData :: Storable a => String -> a -> IO ()
    function addData(nodeName, data) {
	console.log("TODO - add ", data, "to node '", nodeName, "'");
    }

    return {
	addNode: addNode,
	addEdge: addEdge,
	addData: addData,
	refresh: refresh,
	reset: reset
    }
}

document.addEventListener("DOMContentLoaded", function () {
    var g = newGraph("body", 1280, 800)
    var stream = new EventSource("/cl-ring/source");
    stream.onopen = function (m) { console.log("Stream OPENED!")}
    stream.onerror = function (m) { console.log("Stream ERRORED!")}
    stream.onmessage = function (m) {
	var parsed = JSON.parse(m.data)
	if (parsed) {
	    switch (parsed.eventType) {
	    case "addNode":
		g.addNode(parsed.nodeName);
		break;
	    case "addEdge":
		g.addEdge(parsed.from, parsed.to, parsed.label)
		break;
	    case "addData":
		g.addData(parsed.nodeName, parsed.daata)
		break;
	    }
	    g.refresh()
	}
    }
})
