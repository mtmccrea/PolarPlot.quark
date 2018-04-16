PolarView : ValuesView {
	// set in drawFunc, for access by drawing layers
	var <bnds, <cen, <minDim, <canvas;
	// layers
	var <layers, <background, <plots, <grid, <legend, <title, <note;
	var thetas, data; // data points and their corresponding theta positions

	var <direction, <dirFlag;  // dirFlag: cw=1, ccw=-1
	var startAngle, sweepLength, zeroPos;
	var <prZeroPos;     // position of zero used internally, provides the point of reference for prStartAngle
	var <prStartAngle;  // start angle used internally, reference 0 to the RIGHT, as used in addAnnularWedge
	var <prSweepLength; // sweep length used internally, = sweepLength * dirFlag
	var <plotRadius;    // normalized, 1 = outer edge of the view

	// zeroPos reference 0 is UP, advances in direction, in radians
	// startAngle position, reference zeroPos, advances in direction, in radians
	*new {
		|parent, bounds, specs, initVals, startAngle, sweepLength, direction = \cw, zeroPos = 0, plotRadius=0.9|
		^super.new(parent, bounds, specs, initVals).init(startAngle, sweepLength, direction, zeroPos, plotRadius);
	}

	init { |argStartAngle, argSweepLength, argDirection, argZeroPos, argPlotRadius|
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

		zeroPos = argZeroPos;
		direction = argDirection;
		dirFlag = switch (direction, \cw, {1}, \ccw, {-1});
		startAngle = argStartAngle ?? 0; // TODO: revist default start angle, get it from data
		sweepLength = argSweepLength ?? 2pi; // TODO: revist default sweep length, get it from data

		plotRadius = argPlotRadius;

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
	data_ { |thetaArray, rhoArray|

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
			fillColor: Color.white
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
	stroke {}
	fill {}
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