PolarView : ValuesView {
	// set in drawFunc, for access by drawing layers
	var <bnds, <cen, <minDim, <canvas;
	// layers
	var <layers, <background, <plots, <grid, <legend, <title, <note;
	var thetas, data; // data points and their corresponding theta positions

	var <direction, <dirFlag;  // dirFlag: cw=1, ccw=-1
	var startAngle, sweepLength, zeroPos;
	var <prZeroPos;       // position of zero used internally, provides the point of reference for prStartAngle
	var <prStartAngle;    // start angle used internally, reference 0 to the RIGHT, as used in addAnnularWedge
	var <prSweepLength;   // sweep length used internally, = sweepLength * dirFlag
	var <plotRadius;      // normalized, 1 = outer edge of the view
	var <plotUnits;       // units in which the data is plotted
	var <dataUnits;       // units in which the data is provided by user
	var <minVal, <maxVal; // min/maxVal, specified in plotUnits
	var plotMin, plotMax; // min/maxVal, normalized for drawing
	var plotData;         // data to plot, stored as scalar values, normalized to plotMin/Max
	var <gridVals;        // values at which level grid lines are drawn, in plotUnits
	var <plotGrid;        // gridVals, normalized to plotMin/Max
	var gridSpec;

	// zeroPos reference 0 is UP, advances in direction, in radians
	// startAngle position, reference zeroPos, advances in direction, in radians
	// plotUnits = \scalar or \db
	*new {
		|parent, bounds, specs, initVals, startAngle, sweepLength, direction = \cw, zeroPos = 0, plotRadius=0.9, plotUnits = \scalar|
		^super.new(parent, bounds, specs, initVals)
		.init(startAngle, sweepLength, direction, zeroPos, plotRadius, plotUnits);
	}

	init { |argStartAngle, argSweepLength, argDirection, argZeroPos, argPlotRadius, argPlotUnits|
		// REQUIRED: in subclass init, initialize drawing layers
		// initialize layer classes and save them to vars
		#background, grid, plots, legend, title, note = [
			PolarBackgroundLayer, PolarPlotLayer, PolarGridLayer,
			PolarLegendLayer, PolarTxtLayer, PolarTxtLayer
		].collect({ |class|
			class.new(this, class.properties)
		});
		// convenience variable to access a list of the layers
		layers = [background, grid, plots, legend, title, note];

		plotUnits = argPlotUnits;
		zeroPos = argZeroPos;
		direction = argDirection;
		dirFlag = switch (direction, \cw, {1}, \ccw, {-1});
		startAngle = argStartAngle ?? 0; // TODO: revist default start angle, get it from data
		sweepLength = argSweepLength ?? 2pi; // TODO: revist default sweep length, get it from data

		plotRadius = argPlotRadius;

		gridSpec = switch( plotUnits,
			\db, {ControlSpec(-70, 0, \db)},
			\scalar, {ControlSpec(0, 1, \lin)}
		);
		minVal = gridSpec.minval;
		maxVal = gridSpec.maxval;
		plotMin = 0;
		plotMax = 1;
		plotGrid = (plotMin, (plotMax-plotMin/5) .. plotMax);
		gridVals = plotGrid.collect(gridSpec.unmap(_));

		this.direction_(direction); // sets zeroPos, startAngle, sweepLength
		// this.zeroPos_(zeroPos);
		// this.startAngle_(startAngle);
		// this.sweepLength_(sweepLength);

		this.defineMouseActions;
	}

	drawFunc {
		^{|v|
			// "global" instance vars, accessed by ValueViewLayers
			bnds = v.bounds;
			cen  = bnds.center;
			minDim = min(bnds.width, bnds.height);
			// TODO: move setting this variable to onResize function
			canvas = bnds.size.asRect;
			this.drawInThisOrder;
		};
	}

	drawInThisOrder {
		// tranlate to top left of drawing area
		Pen.translate(canvas.leftTop.x, canvas.leftTop.y);
		if (background.p.show) {background.fill};
		if (grid.p.show) {grid.stroke};
		if (plots.p.show) {plots.stroke};
		if (legend.p.show) {legend.stroke};
		if (title.p.show) {title.stroke};
		if (note.p.show) {note.stroke};
	}

	// TODO: allow 2D arrays array for multiple plots overlaid
	data_ { |thetaArray, rhoArray, units = \scalar|

		if (thetaArray.size != rhoArray.size) {
			// thetaArray assumed to be breakpoints (likely start/end points)
			// create an "envelope" from breakpoints and generate theta values
			// for each rho value
			var thetaEnv, dataStep;
			dataStep = (rhoArray.size - 1).reciprocal;
			thetaEnv = Env(thetaArray, (thetaArray.size-1).reciprocal, \lin);
			thetaArray = rhoArray.collect{|val, i| thetaEnv.at(dataStep*i)};
		};

		thetas = thetaArray;
		data = rhoArray;

		dataUnits = units;

		// plotData always stored as scalar values, convert if needed
		plotData = if (dataUnits == \db, {rhoArray.dbamp}, {rhoArray});
		this.prStorePlotMinMax;

		// re-calc plotData based on min/max
		this.minMax_(minVal, maxVal, true);
	}

	// preferred way of setting min and max together
	// min and max should be set in plotUnits
	minMax_ { |min, max, refresh=true|
		this.minVal_(min, false, false);
		this.maxVal_(max, true, true); // rescale data and refresh
	}

	// minVal should be set in plotUnits
	// use minMax_ is setting both min and max
	minVal_ { |min, rescaleNow=true, refresh=true|
		minVal = min;
		plotMin = if ((min == \auto) or: min.isNil) {
			plotData.minItem;  // detect max from data
		} { // max is provided
			if (plotUnits == \db)
			{ min.dbamp} // convert plotUnits to scalar if needed
			{ min }
		};

		rescaleNow.if{this.prRescalePlotData(refresh)};
	}

	// maxVal should be set in the plotUnits
	// use minMax_ is setting both min and max
	maxVal_ { |max, rescaleNow=true, refresh=true|
		maxVal = max;
		plotMax = if ((max == \auto) or: max.isNil) {
			plotData.maxItem;  // detect max from data
		} { // max is provided
			if (plotUnits == \db)
			{ max.dbamp} // convert plotUnits to scalar if needed
			{ max }
		};

		rescaleNow.if{this.prRescalePlotData(refresh)};
	}

	// \db or \scalar
	plotUnits_ { |dbOrScalar|
		if (plotUnits != dbOrScalar) {
			plotUnits = dbOrScalar;

			// update min/max
			minVal !? {
				this.minVal_(
					if (plotUnits == \db, {minVal.ampdb}, {minVal.dbamp}),
					rescaleNow: false, refresh: false  // rescale, refresh later
				);
			};

			maxVal !? {
				this.maxVal_(
					if (plotUnits == \db, {maxVal.ampdb}, {maxVal.dbamp}),
					rescaleNow: false, refresh: false  // rescale, refresh later
				);
			};

			// reset the grid lines to new plotUnits
			this.levelGridLinesAt_(
				if (plotUnits == \db, {gridVals.ampdb}, {gridVals.dbamp}),
				false  // refresh later
			);

			this.prRescalePlotData(refresh:true)
		};
	}

	// called from other methods which set new plotMin/Max
	prRescalePlotData { |refresh=true|
		var curPlotMin, curPlotMax;

		plotData ?? {"data has not yet been set".warn; ^this};

		curPlotMin = plotData.minItem;
		curPlotMax = plotData.maxItem;

		plotData = plotData.linlin(
			curPlotMin, curPlotMax,
			plotMin, plotMax
		);
		this.prStorePlotMinMax;


		refresh.if{this.refresh};
	}

	defineMouseActions {
		// mouseDownAction = { |v, x, y| };
		// mouseMoveAction = { |v, x, y| };
	}

	plotRadius_ { |normRadius|
		plotRadius = normRadius;
		this.refresh;
	}

	direction_ { |dir=\cw|
		direction = dir;
		dirFlag = switch (direction, \cw, {1}, \ccw, {-1});
		this.sweepLength_(sweepLength, false);
		this.zeroPos_(zeroPos, false);  // updates prZeroPos, startAngle
		this.refresh;
	}

	// This serves as the reference for start angle
	// When setting this value, reference 0 up, direction \cw
	zeroPos_ { |radians=0, refresh=true|
		zeroPos = radians;
		prZeroPos = -0.5pi + (radians * dirFlag);
		this.startAngle_(startAngle, false);
		refresh.if{this.refresh};
	}

	// startAngle is from zeroDirection, advancing in direction
	startAngle_ {|radians=0, refresh=true|
		startAngle = radians;
		prStartAngle = prZeroPos + (startAngle*dirFlag);
		refresh.if{this.refresh};
	}

	sweepLength_ { |radians=2pi, refresh=true|
		sweepLength = radians;
		prSweepLength = sweepLength * dirFlag;
		refresh.if{this.refresh};
	}

	// in plotUnits, grid lines created from "from",
	// stepping "every", until reaching "until"
	levelGridLines_ { |from, every, until, refresh=true|
		var f, e, u, step;
		f = switch(from,
			\max, {plotMax},
			\min, {plotMin},
			{from}
		);
		u = switch(until,
			\max, {plotMax},
			\min, {plotMin},
			{until}
		);
		step = if (f<u) {every} {every.neg};
		gridVals = (f, f+step .. u);
		this.levelGridLinesAt_(gridVals, refresh);
	}

	// an array of levels for the grid lines, in plotUnits
	levelGridLinesAt_ { |levelArray, refresh|
		gridVals = levelArray;
		plotGrid = if (plotUnits == \db) {gridVals.dbamp} {gridVals};
		refresh.if{this.refresh};
	}
}

PolarBackgroundLayer  : ValueViewLayer {

	*properties {
		^(
			show:      true,
			fillColor: Color.white,
		)
	}

	fill {
		Pen.push;
		Pen.fillColor_(p.fillColor);
		Pen.addAnnularWedge(view.cen, 0.001, view.minDim/2*view.plotRadius, view.prStartAngle, view.prSweepLength);
		Pen.fill;
		Pen.pop;
	}
}


/* drawing layers */

PolarPlotLayer : ValueViewLayer {
	*properties {
		^(
			show:      true,
			fillColor: Color.white,
			plotColors: Color.blue,
		)
	}

	stroke {}

	fill {}
}

PolarGridLayer : ValueViewLayer {
	*properties {
		^(
			show:      true,
			showLonLines: true,
			showLatLines: true,
			strokeColor: Color.gray.alpha_(0.4),
			lonSpacing: 30.degrad,
			latSpacing: -10.dbamp,
			showLonVals: true,
			showLatVals: true,
		)
	}

	stroke {
		var rad = view.minDim/2*view.plotRadius;

		Pen.push;
		Pen.strokeColor_(p.strokeColor);
		view.plotGrid.do{ |level|
			Pen.addArc(view.cen, level * rad, view.prStartAngle, view.prSweepLength);
		};
		Pen.stroke;
		Pen.pop;
	}
}

PolarLegendLayer : ValueViewLayer {
	*properties {
		^(
			show:      true,
			fillColor: Color.white
		)
	}
	stroke {}
	fill {}
}

PolarTxtLayer : ValueViewLayer {
	*properties {
		^(
			show:      true,
			fillColor: Color.white
		)
	}
	stroke {}
	fill {}
}