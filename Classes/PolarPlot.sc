PolarView : ValuesView {
	// set in drawFunc, for access by drawing layers
	var <bnds, <cen, <minDim, <canvas;
	// layers
	var <background, <plots, <grid, <legend, <title, <note;
	var <thetas, data; // data points and their corresponding theta positions

	var <direction, <dirFlag;  // dirFlag: cw=1, ccw=-1
	var startAngle, sweepLength, zeroPos;
	var <prZeroPos;       // position of zero used internally, provides the point of reference for prStartAngle
	var <prStartAngle;    // start angle used internally, reference 0 to the RIGHT, as used in addAnnularWedge
	var <prSweepLength;   // sweep length used internally, = sweepLength * dirFlag
	var <plotRadius;      // normalized, 1 = outer edge of the view
	var <plotUnits;       // units in which the data is plotted
	var <dataUnits;       // units in which the data is provided by user
	var <minVal, <maxVal; // min/maxVal, specified in plotUnits
	var <plotData;        // data to plot, stored as scalar values, normalized to plotMin/Max
	var <plotSpec;        // ControlSpec that maps data to plotting range
	var <dataScalar;      // input data, stored as scalar values
	var dataMin, dataMax; // min/max of input data, in scalar values
	var <gridVals;        // values at which level grid lines are drawn, in plotUnits
	var <plotGrid;        // gridVals, normalized to plotMin/Max
	var <thetaLines;      // theta positions of longitude lines, relative to 'zeroPos' and 'direction;
	var <prThetaLines;    // theta positions of longitude lines, in drawing coordinates

	var <>clipDbLow = -90; // min dB level when setting plot minimum (0.ampdb = -inf, which breaks ControlSpec);

	// zeroPos reference 0 is UP, advances in direction, in radians
	// startAngle position, reference zeroPos, advances in direction, in radians
	// plotUnits = \scalar or \db
	*new {
		|parent, bounds, specs, initVals, startAngle, sweepLength, direction = \cw, zeroPos = \up, plotRadius=0.9, plotUnits = \scalar|
		^super.new(parent, bounds, specs, initVals)
		.init(startAngle, sweepLength, direction, zeroPos, plotRadius, plotUnits);
	}

	init { |argStartAngle, argSweepLength, argDirection, argZeroPos, argPlotRadius, argPlotUnits|

		// REQUIRED: in subclass init, initialize drawing layers
		layers = [
			PolarBackgroundLayer, PolarPlotLayer, PolarGridLayer,
			PolarLegendLayer, PolarTxtLayer, PolarTxtLayer
		].collect({ |class|
			class.new(this, class.properties)
		});

		// unpack the layers list into individual variables
		#background, plots, grid, legend, title, note = layers;
		// layers = [background, grid, plots, legend, title, note];

		plotUnits = argPlotUnits;
		direction = argDirection;
		dirFlag = switch (direction, \cw, {1}, \ccw, {-1});
		zeroPos = if (argZeroPos.isKindOf(Number)) {argZeroPos} {
			switch(argZeroPos,
				\up, {0},
				\down, {pi},
				\left, {-0.5pi * dirFlag},
				\right, {0.5pi * dirFlag}
			)
		};
		startAngle = argStartAngle ?? 0; // TODO: revist default start angle, get it from data
		sweepLength = argSweepLength ?? 2pi; // TODO: revist default sweep length, get it from data

		plotRadius = argPlotRadius;

		plotSpec = switch(plotUnits, \db, {[-70, 0, \lin]}, \scalar, {[0,1]}).asSpec;
		gridVals = (plotSpec.minval, plotSpec.minval + ((plotSpec.maxval-plotSpec.minval)/5) .. plotSpec.maxval);
		plotGrid = gridVals.collect(plotSpec.unmap(_));
		dataMin = plotSpec.minval;
		dataMax = plotSpec.maxval;

		plotSpec.postln;
		plotGrid.postln;
		gridVals.postln;

		this.direction_(direction); // sets zeroPos, startAngle, sweepLength
		this.thetaGridLines_(pi/4, false);
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
		if (legend.p.show) {legend.fill; legend.stroke};
		if (title.p.show) {title.fill; title.stroke;};
		// if (note.p.show) {note.stroke};
	}

	// TODO: How to handle negative values? (e.g. phase of figure of 8 polar pattern...)
	//    always .abs?
	//    through origin? if through origin, alter color based on sign/phase?
	data_ { |thetaArray, rhoArray, units = \scalar, refresh=true|
		var shapetest;

		// support 2D arrays: force into shape [1, datasize], i.e. [[1,2,3],[1,3,7]]
		if (rhoArray.shape.size == 1) {
			rhoArray = [rhoArray];
		};
		// check that multiple datasets are the same size
		// TODO: don't make this a requirement
		shapetest = rhoArray.collect(_.size).every(_ == rhoArray[0].size);
		shapetest.not.if{warn(format("Size of datasets are mismatched: %", rhoArray.collect(_.size)))};


		dataScalar = if (units==\db) {rhoArray.dbamp} {rhoArray};
		dataMin = dataScalar.flat.minItem;
		dataMax = dataScalar.flat.maxItem;

		if (thetaArray.size != rhoArray[0].size) {
			// thetaArray assumed to be breakpoints (likely start/end points)
			// create an "envelope" from breakpoints and generate theta values
			// for each rho value
			var thetaEnv, dataStep;
			dataStep = (rhoArray[0].size - 1).reciprocal;
			thetaEnv = Env(thetaArray, (thetaArray.size-1).reciprocal, \lin);
			thetaArray = rhoArray[0].collect{|val, i| thetaEnv.at(dataStep*i)};
		};

		thetas = thetaArray;

		// re-calc plotData based on min/max
		this.plotMinMax_(minVal, maxVal, refresh);
	}

	// preferred way of setting min and max together
	// min and max should be set in plotUnits
	plotMinMax_ { |min, max, refresh=true|
		this.plotMin_(min, false, false);
		this.plotMax_(max, true, refresh); // rescale data and refresh
	}

	plotMin {^plotSpec.minval}
	plotMax {^plotSpec.maxval}

	// min should be set in plotUnits
	// use plotMinMax_ is setting both min and max
	plotMin_ { |min, rescaleNow=true, refresh=true|
		var plotMin;

		plotMin = if ((min == \auto) or: min.isNil) {
			if (plotUnits == \db) {max(dataMin.ampdb, clipDbLow)} {dataMin}
		} {
			min
		};

		// "plotMin updated to: ".post; plotMin.postln;

		this.plotSpec_(
			ControlSpec.newFrom(plotSpec).minval_(plotMin),
			rescaleNow,
			refresh
		);

		this.levelGridLinesAt_(this.gridVals, false);
		rescaleNow.if{this.prRescalePlotData(refresh)};
	}

	// max should be set in the plotUnits
	// use plotMinMax_ is setting both min and max
	plotMax_ { |max, rescaleNow=true, refresh=true|
		var plotMax;

		plotMax = if ((max == \auto) or: max.isNil) {
			if (plotUnits == \db) {dataMax.ampdb} {dataMax}
		} {
			max
		};

		// "plotMax updated to: ".post; plotMax.postln;

		this.plotSpec_(
			ControlSpec.newFrom(plotSpec).maxval_(plotMax),
			rescaleNow,
			refresh
		);

		this.levelGridLinesAt_(this.gridVals, false);
		rescaleNow.if{this.prRescalePlotData(refresh)};
	}

	// \db or \scalar
	plotUnits_ { |dbOrScalar, min, max|
		var newMin, newMax;
		if (plotUnits != dbOrScalar) {
			plotUnits = dbOrScalar;

			if (plotUnits == \db) {
				// switched to \db scaling
				newMin = min ?? {max(plotSpec.minval.ampdb, clipDbLow)};
				newMax = max ?? plotSpec.maxval.ampdb;
			} { // switched to \scalar scaling from \db
				newMin = min ?? plotSpec.minval.dbamp;
				newMax = max ?? plotSpec.maxval.dbamp;
			};

			// updates plotData
			this.plotSpec_(
				// ControlSpec(newMin, newMax, switch(dbOrScalar, \db, {\db}, \scalar, {\lin})),
				ControlSpec(newMin, newMax, plotSpec.warp), // spec always linear, even if data is \db
				true, false // rescale now, don't refresh yet
			);

			// reset the grid lines to new plotUnits
			this.levelGridLinesAt_(
				if (plotUnits == \db, {gridVals.ampdb}, {gridVals.dbamp}).postln,
				true  // refresh
			);
		};
	}

	plotSpec_ { |spec, rescaleNow, refresh|
		plotSpec = spec;
		// this.levelGridLinesAt_(gridVals, false);
		"new spec: ".post; spec.postln;

		// TODO: handle switching the plotUnits with the new spec

		rescaleNow.if{this.prRescalePlotData(refresh:true)};
	}

	// TODO
	plotWarp_ { |warp|
		"updating warp not yet implemented".warn;
		// plotSpec.warp_(warp.asWarp);
		// update data,
		// update gridlines
	}

	// called from other methods which set new plotMin/Max
	prRescalePlotData { |refresh=true|
		dataScalar ?? {"data has not yet been set".warn; ^this};
		"rescaling plot data: ".postln;

		plotData = if (plotUnits == \db) {
			// dataScalar is scalar, spec is in db, so convert unmap
			"plot units are db, unmapping from plotSpec .ampdb".postln;
			// dataScalar.ampdb.postln;
			plotSpec.copy.unmap(dataScalar.ampdb);
		} {
			"plot units are scalar, unmapping from plotSpec".postln;
			// dataScalar.postln;

			plotSpec.copy.unmap(dataScalar);
		};

		"rescaled plot data: ".postln; plotData.postln;

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
	zeroPos_ { |radiansOrDir=0, refresh=true|
		zeroPos = if (radiansOrDir.isKindOf(Number)) {
			radiansOrDir
		} {
			switch(radiansOrDir,
				\up, {0}, \down, {pi}, \left, {-0.5pi}, \right, {0.5pi}
			)
		};
		// zeroPos = radians;
		prZeroPos = -0.5pi + (zeroPos * dirFlag);
		this.startAngle_(startAngle, false);
		refresh.if{this.refresh};
	}

	// startAngle is from zeroDirection, advancing in direction
	startAngle_ {|radians=0, refresh=true|
		startAngle = radians;
		prStartAngle = prZeroPos + (startAngle*dirFlag);
		thetaLines !? {
			this.thetaGridLinesAt_(thetaLines, false); // reset the thera grid lines
		};
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
		var f, u, step;
		f = switch(from,
			\max, {this.plotMax},
			\min, {this.plotMin},
			{from}
		);
		u = switch(until,
			\max, {this.plotMax},
			\min, {this.plotMin},
			{until}
		);
		step = if (f<u) {every} {every.neg};
		gridVals = (f, f+step .. u);
		this.levelGridLinesAt_(gridVals, refresh);
	}

	// an array of levels for the grid lines, in plotUnits
	levelGridLinesAt_ { |levelArray, refresh|
		// clip to plotMin/Max
		gridVals = levelArray.select{ |level|
			level.inRange(this.plotMin, this.plotMax)
		};

		plotGrid = plotSpec.copy.unmap(gridVals); // TODO: why is .copy required??? it fails without it, bug?
		postf("plotSpec: %\ngridVals: %\n", plotSpec, gridVals.round(0.00001), plotGrid.round(0.00001));
		// map to plot normalization

		refresh.if{this.refresh};
	}

	// spacing in radians (currently)
	thetaGridLines_ { |spacing, refresh=true|

		thetaLines = (startAngle, startAngle + spacing .. startAngle + sweepLength);

		// avoid duplicate lines at 0, 2pi
		if ((thetaLines.first % 2pi) == (thetaLines.last % 2pi), {
			thetaLines = thetaLines.keep(thetaLines.size-1)
		});

		this.thetaGridLinesAt_(thetaLines, refresh);
	}

	// an array of thetaPositions, relative to 'zeroPos' and 'direction'
	thetaGridLinesAt_ { |thetaArray, refresh|

		// clip to range of plot
		thetaLines = thetaArray.select{ |theta|
			theta.inRange(startAngle, startAngle+sweepLength)
		};

		prThetaLines = prZeroPos + (thetaLines * dirFlag);
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
			plotColors: [Color.blue],
			strokeWidth: 2,
		)
	}

	stroke {
		var zeroPos, dirFlag, maxRad;
		zeroPos = view.prZeroPos;
		dirFlag = view.dirFlag;
		maxRad = view.minDim/2*view.plotRadius;

		Pen.push;
		Pen.translate(view.cen.x, view.cen.y);

		view.plotData.do{ |dataset, j|
			Pen.push;
			Pen.moveTo(
				Polar(dataset[0] * maxRad, zeroPos + (view.thetas[0] * dirFlag)).asPoint
			);
			dataset.do{|val, i|
				Pen.lineTo(
					Polar(val * maxRad, zeroPos + (view.thetas[i] * dirFlag)).asPoint
				);
			};
			Pen.strokeColor_(p.plotColors.asArray.wrapAt(j));
			Pen.width_(p.strokeWidth);
			Pen.stroke;
			Pen.pop;
		};
		Pen.pop;
	}

	fill {}
}

PolarGridLayer : ValueViewLayer {
	*properties {
		^(
			show:         true,
			showLonLines: true,
			showLatLines: true,
			strokeColor:  Color.gray.alpha_(0.4), // grid color
			showLonVals:  true,
			showLatVals:  true,
			latTxtColor:  Color.gray.alpha_(0.3),
			lonTxtColor:  Color.black,
			latTxtRound:  0.01,
			lonTxtRound:  1,
			latTxtPos:    pi/4, // radian angle of latitude labels, relative to zeroPos
			strokeWidth:  1,
		)
	}

	stroke {
		var rad = view.minDim/2*view.plotRadius;

		Pen.push;

		/* lattitude lines */
		Pen.strokeColor_(p.strokeColor);
		Pen.width_(p.strokeWidth);

		if (p.showLatLines) {
			view.plotGrid.do{ |level|
				Pen.addArc(view.cen, level * rad, view.prStartAngle, view.prSweepLength);
			};
			Pen.stroke;
		};

		if (p.showLonLines) {
			Pen.push;
			Pen.translate(view.cen.x, view.cen.y);

			view.prThetaLines.do{ |theta|
				Pen.moveTo(0@0);
				Pen.lineTo(Polar(rad, theta).asPoint);
			};
			Pen.stroke;
			Pen.pop;
		};


		/* lattitude labels */
		if(p.showLatVals) {
			var str, theta, strBnds, corner, thetaMod;

			Pen.push;
			Pen.translate(view.cen.x, view.cen.y);

			theta = view.prZeroPos + (p.latTxtPos * view.dirFlag);
			thetaMod = theta % 2pi;

			view.plotGrid.do{ |level, i|
				corner = Polar(level * rad, theta).asPoint;
				str = view.gridVals[i].round(p.latTxtRound).asString;
				strBnds = (str.bounds.asArray + [0,0,2,2]).asRect;

				if (thetaMod.inRange(0,pi),
					{ strBnds.bottom_(corner.y) },
					{ strBnds.top_(corner.y) }
				);

				if ((thetaMod < 0.5pi) or: (thetaMod > 1.5pi),
					{ strBnds.right_(corner.x) },
					{ strBnds.left_(corner.x) }
				);

				Pen.stringCenteredIn(
					str,
					// str.bounds.top_(0).right_(level * rad),
					// str.bounds.top_(corner.y).right_(corner.x),
					strBnds,
					Font("Helvetica", 12), p.latTxtColor
				);
				// Pen.fillOval(Size(5,5).asRect.center_(corner)); Pen.fill;
			};
			Pen.pop;
		};
		Pen.pop;

		/* longitude labels */
		if(p.showLonVals) {
			var str, theta, strBnds, corner, thetaMod;

			Pen.push;
			Pen.translate(view.cen.x, view.cen.y);


			view.prThetaLines.do{ |theta, i|
				thetaMod = theta % 2pi;

				corner = Polar(rad, theta).asPoint;
				str = view.thetaLines[i].raddeg.round(p.lonTxtRound).asString;
				strBnds = (str.bounds.asArray + [0,0,2,2]).asRect;

				if (thetaMod.inRange(0,pi),
					{ strBnds.top_(corner.y) },
					{ strBnds.bottom_(corner.y) }
				);

				if ((thetaMod < 0.5pi) or: (thetaMod > 1.5pi),
					{ strBnds.left_(corner.x) },
					{ strBnds.right_(corner.x) }
				);

				Pen.stringCenteredIn(
					str,
					// str.bounds.top_(0).right_(level * rad),
					// str.bounds.top_(corner.y).right_(corner.x),
					strBnds,
					Font("Helvetica", 12), p.lonTxtColor
				);
				// Pen.fillOval(Size(5,5).asRect.center_(corner)); Pen.fill;
			};
			Pen.pop;
		};
		Pen.pop;
	}
}

PolarLegendLayer : ValueViewLayer {
	var nElem, txtRects, lRect, labels, font;
	var updated = false;

	*properties {
		^(
			show:        true,
			fillColor:   Color.white,
			txtColor:    Color.gray,
			align:      \bottomRight, // right, left, top, bottom, topRight, topLeft, bottomRight, bottomLeft
			margin:      10,           // margin between legend and view's edge
			pad:         5,            // padding between text and legend border
			layout:      \vertical,    // horizontal, vertical
			lineLength:  15,           // length of sample plot line
			lineSpacing: 4,            // spacing between sample line and text
			font:        Font("Helvetica"),
			fontSize:    14,
			labels:      ["plot 1"],
			showBorder:  true,
			borderColor: Color.gray,
			borderWidth: 2,
		)
	}

	stroke {
		var lineCols, cursor, h_2, yHop, stX;
		"nElem in ".post; nElem.postln;
		block {|break|

			lRect ?? {break.()}; // bail if background hasn't been calculated

			Pen.push;

			Pen.translate(view.cen.x, view.cen.y);    // set 0,0 to center

			if (p.showBorder) {
				Pen.width_(p.borderWidth);
				Pen.strokeColor_(p.borderColor);
				Pen.addRect(lRect);
				Pen.stroke;
			};

			cursor = lRect.leftTop + (p.pad@p.pad);
			Pen.translate(cursor.x, cursor.y);        // translate to top left of legend background rect
			cursor = 0@0; // reset cursor

			Pen.width = view.plots.strokeWidth;
			lineCols = view.plots.plotColors;
			h_2 = txtRects[0].height/2;

			switch(p.layout,
				\horizontal, { // w = pad-lineLength-lineSpacing-text length-pad-lineLength-lineSpacing-text length-pad, etc
					Pen.push;
					nElem.do{ |i|
						Pen.strokeColor_(lineCols.wrapAt(i));

						Pen.moveTo(Point(cursor.x,cursor.y+h_2));
						cursor.x = cursor.x + p.lineLength;
						Pen.lineTo(Point(cursor.x,cursor.y+h_2));
						Pen.stroke;

						cursor.x = cursor.x + p.lineSpacing;
						Pen.stringLeftJustIn(
							labels[i],
							txtRects[i].left_(cursor.x).top_(cursor.y),
							font,
							p.txtColor
						);
						cursor.x = cursor.x + txtRects[i].width + p.pad;
					};
					Pen.pop;
				},
				\vertical, { // h = pad-txtHeight-pad-txtHeight-pad
					Pen.push;
					yHop = txtRects[0].height+ p.pad;
					stX = cursor.x;
					nElem.do{ |i|
						Pen.strokeColor_(lineCols.wrapAt(i));

						Pen.moveTo(Point(stX,cursor.y+h_2));
						cursor.x = stX + p.lineLength;
						Pen.lineTo(Point(cursor.x,cursor.y+h_2));
						Pen.stroke;

						cursor.x = cursor.x + p.lineSpacing;
						Pen.stringLeftJustIn(
							labels[i],
							txtRects[i].left_(cursor.x).top_(cursor.y),
							font,
							p.txtColor
						);
						cursor.x = stX; // jump back to starting x
						cursor.y = cursor.y + yHop;
					};
					Pen.pop;
				}
			);

			Pen.pop;
		};
	}

	fill {
		var sumW, sumH, pad;

		nElem = view.plotData.size;
		if(nElem > 0) {
			Pen.push;
			Pen.translate(view.cen.x, view.cen.y);

			labels = p.labels.asArray.extend(nElem, " - "); // make sure there are labels for all plots
			font = p.font.size_(p.fontSize);
			txtRects = labels.collect(_.bounds(font));
			pad = p.pad;

			switch(p.layout,
				\horizontal, { // w = pad-lineLength-lineSpacing-textlength-pad-lineLength-lineSpacing-text length-pad, etc
					sumW = (pad + p.lineLength + p.lineSpacing * nElem) + txtRects.collect(_.width).sum + pad;
					sumH = (pad * 2) + txtRects[0].height;
				},
				\vertical, { // h = pad-txtHeight-pad-txtHeight-pad
					sumW = pad + p.lineLength + p.lineSpacing + txtRects.collect(_.width).maxItem + pad;
					sumH = pad + (txtRects[0].height + pad * nElem);
				}
			);

			lRect = [0,0, sumW, sumH].asRect.center_(0@0);

			switch(p.align,
				\right, {lRect.right = view.bnds.width/2 - p.margin},
				\left, {lRect.left = view.bnds.width/2.neg + p.margin},
				\top, {lRect.top = view.bnds.height/2.neg + p.margin},
				\bottom, {lRect.bottom = view.bnds.height/2 - p.margin},
				\topRight, {
					lRect.top = view.bnds.height/2.neg + p.margin;
					lRect.right = view.bnds.width/2 - p.margin;
				},
				\topLeft, {
					lRect.top = view.bnds.height/2.neg + p.margin;
					lRect.left = view.bnds.width/2.neg + p.margin;
				},
				\bottomRight, {
					lRect.bottom = view.bnds.height/2 - p.margin;
					lRect.right = view.bnds.width/2 - p.margin;
				},
				\bottomLeft, {
					lRect.bottom = view.bnds.height/2 - p.margin;
					lRect.left = view.bnds.width/2.neg + p.margin;
				},
			);

			Pen.fillColor_(p.fillColor);
			Pen.fillRect(lRect);
			Pen.fill;
			Pen.pop;
		}
	}
}

PolarTxtLayer : ValueViewLayer {
	var font, txtRect, bgRect;

	*properties {
		^(
			show:        true,
			fillColor:   Color.white,
			txtColor:    Color.black,
			align:       \top,
			margin:      15,
			pad:         15,
			txt:         "plot",
			font:        Font("Helvetica"),
			fontSize:    18,
			showBorder:  true,
			borderColor: Color.gray,
			borderWidth: 2,
		)
	}

	stroke {
		Pen.push;

		if (p.showBorder) {
			Pen.width_(p.borderWidth);
			Pen.strokeColor_(p.borderColor);
			Pen.addRect(bgRect);
			Pen.stroke;
		};

		// r = (view.canvas.asArray * [1, 1, p.width, p.height] + [m,m,m.neg,m.neg]).asRect.center_(view.cen.x@(view.bnds.height*p.height/2));
		txtRect = txtRect.center_(bgRect.center);
		Pen.stringCenteredIn(p.txt, txtRect, font, p.txtColor);
		Pen.stroke;
		Pen.pop;
	}

	fill {
		font = p.font.size_(p.fontSize);
		txtRect = p.txt.bounds(p.font.size_(p.fontSize));
		bgRect = txtRect + [0, 0, p.pad, p.pad].asRect;

		Pen.push;
		// r = (view.canvas.asArray * [1, 1, p.width, p.height] + [m,m,m.neg,m.neg]).asRect.center_(view.cen.x@(view.bnds.height*p.height/2));
		bgRect = bgRect.center_(view.bnds.width/2 @ (bgRect.height/2 + p.margin));
		Pen.fillColor_(p.fillColor);
		Pen.fillRect(bgRect);
		Pen.fill;
		Pen.pop;
	}
}