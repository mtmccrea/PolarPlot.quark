/*
TODO
- \circle data points, for the option of re-creating rE-style directivity displays
- invert values so that minimum values are at the radius, max values are at the center
- support a notion of phase, as in a figure-of-eight mic pattern
- optionally resample the data when plotting for radially uniform sample points, instead of
-      one point per data point
- calculate plot/view dimensions based on actual plot outline (if it's not a circle)
-      e.g. a semi-circular or wedge plot
- optionally show lattitude scale in a separate, adjacent view that aligns with plot
*/

PolarView : ValuesView {
	// set in drawFunc, for access by drawing layers
	var <bnds, <cen, <minDim;
	// layers
	var <background, <plots, <grid, <legend, <title;
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
	var <prPlotCen;       // center of the plot, offset by the title
	var <prPlotRad;       // radius of the plot, scaled by plotRadius arg

	var <>clipDbLow = -90; // min dB level when setting plot minimum (0.ampdb = -inf, which breaks ControlSpec);
	var <displayAbsoluteValue = false; // display the data as the absolute values (of the scalar values)

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
			PolarLegendLayer, PolarTitleLayer
		].collect({ |class|
			class.new(this, class.properties)
		});

		// unpack the layers list into individual variables
		#background, plots, grid, legend, title = layers;

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
		startAngle = argStartAngle ?? 0;     // TODO: revist default start angle, get it from data
		sweepLength = argSweepLength ?? 2pi; // TODO: revist default sweep length, get it from data

		plotRadius = argPlotRadius;

		plotSpec = switch(plotUnits, \db, {[-70, 0, \lin]}, \scalar, {[0,1]}).asSpec;
		gridVals = (plotSpec.minval, plotSpec.minval + ((plotSpec.maxval-plotSpec.minval)/5) .. plotSpec.maxval);
		plotGrid = gridVals.collect(plotSpec.unmap(_));
		dataMin = plotSpec.minval;
		dataMax = plotSpec.maxval;

		this.direction_(direction); // sets zeroPos, startAngle, sweepLength
		this.thetaGridLines_(pi/4, false);
		this.defineMouseActions;
	}

	drawFunc {
		^{  |v|
			var titleOffset, h;
			// "global" instance vars, accessed by ValueViewLayers
			bnds = v.bounds;
			if (title.p.show) {
				title.calcBounds;
				titleOffset = 2 * title.p.inset + title.bgRect.height;
				h = bnds.height - titleOffset;
			} {
				titleOffset = 0;
				h = bnds.height
			};
			cen = bnds.center;
			minDim = min(bnds.width, h);      // scale down for space around edge
			prPlotRad = minDim / 2 * plotRadius;
			prPlotCen = Point(cen.x, max(cen.y, titleOffset + (minDim/2)));

			this.drawInThisOrder;
		};
	}

	drawInThisOrder {
		if (background.p.show) {background.fill};
		if (grid.p.show) {grid.stroke};
		if (plots.p.show) {plots.stroke};
		if (legend.p.show) {legend.fill; legend.stroke};
		if (title.p.show) {title.fill; title.stroke;};
	}

	// TODO: How to handle negative values? (e.g. phase of figure of 8 polar pattern...)
	//    always .abs?
	//    through origin? if through origin, alter color based on sign/phase?
	data_ { |thetaArray, rhoArray, units = \scalar, displayAbs, refresh=true|
		var shapetest, dataSizes;
		var thetaEnv, dataStep;
		// var flatData, thetaEnv, dataStep;

		// support 2D arrays: force into shape [1, datasize], i.e. [[1,2,3],[1,3,7]]
		if (rhoArray.shape.size == 1) {
			rhoArray = [rhoArray];
		};

		// check that multiple datasets are the same size
		// TODO: don't make this a requirement
		dataSizes = rhoArray.collect(_.size);
		shapetest = dataSizes.every(_ == rhoArray[0].size);
		shapetest.not.if{
			warn(format(
				"[PolarView:-data_] Size of datasets are mismatched: "
				"%, extending datasets for identical sizes",
				rhoArray.collect(_.size))
			);
			// stutter the last values to make all array sizes equal
			rhoArray = rhoArray.collect{|arr| arr.extend(dataSizes.maxItem, arr.last)};
		};

		dataScalar = if (units==\db) { rhoArray.dbamp } { rhoArray };

		if (thetaArray.shape.size == 1) {
			thetaArray = [thetaArray];
		};

		thetaArray.do{ |thetaArr, i|

			if (thetaArr.size != rhoArray[i].size) {
				// thetaArray assumed to be breakpoints (likely start/end points)
				// create an "envelope" from breakpoints and generate theta values
				// for each rho value
				dataStep = (rhoArray[i].size - 1).reciprocal;
				thetaEnv = Env(thetaArr, (thetaArr.size-1).reciprocal, \lin);
				thetaArray[i] = rhoArray[i].collect{|val, j| thetaEnv.at(dataStep*j)};
			};
		};

		thetas = thetaArray;

		// set dataMin/Max based on displayAbsoluteValue flag
		this.displayAbsoluteValue_(displayAbs ?? displayAbsoluteValue, refresh);
	}

	displayAbsoluteValue_ { |bool, refresh=true|
		var flatData;

		displayAbsoluteValue = bool;

		flatData = dataScalar.flat;
		if (displayAbsoluteValue) { flatData = flatData.abs };

		dataMin = flatData.minItem;
		dataMax = flatData.maxItem;

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

		plotMin = if (min == \auto or: { min.isNil }) {
			if (plotUnits == \db) { max(dataMin.ampdb, clipDbLow) } { dataMin }
		} {
			min
		};

		this.plotSpec_(
			ControlSpec.newFrom(plotSpec).minval_(plotMin),
			rescaleNow,
			refresh
		);

		this.levelGridLinesAt_(this.gridVals, false);
		rescaleNow.if{ this.prRescalePlotData(refresh) };
	}

	// max should be set in the plotUnits
	// use plotMinMax_ is setting both min and max
	plotMax_ { |max, rescaleNow=true, refresh=true|
		var plotMax;

		plotMax = if (max == \auto or: { max.isNil }) {
			if (plotUnits == \db) { dataMax.ampdb } { dataMax }
		} {
			max
		};

		this.plotSpec_(
			ControlSpec.newFrom(plotSpec).maxval_(plotMax),
			rescaleNow,
			refresh
		);

		this.levelGridLinesAt_(this.gridVals, false);
		rescaleNow.if{ this.prRescalePlotData(refresh) };
	}

	// \db or \scalar
	plotUnits_ { |dbOrScalar, min, max|
		var newMin, newMax;
		if (plotUnits != dbOrScalar) {
			plotUnits = dbOrScalar;

			if (plotUnits == \db) {
				// switched to \db scaling
				newMin = min ?? { max(plotSpec.minval.ampdb, clipDbLow) };
				newMax = max ?? plotSpec.maxval.ampdb;
			} { // switched to \scalar scaling from \db
				newMin = min ?? plotSpec.minval.dbamp;
				newMax = max ?? plotSpec.maxval.dbamp;
			};

			// updates plotData
			this.plotSpec_(
				ControlSpec(newMin, newMax, plotSpec.warp),  // spec always linear, even if data is \db
				true, false                                  // rescale now, don't refresh yet
			);

			// reset the grid lines to new plotUnits
			this.levelGridLinesAt_(
				if (plotUnits == \db) { gridVals.ampdb } { gridVals.dbamp },
				true  // refresh
			);
		};
	}

	plotSpec_ { |spec, rescaleNow, refresh|
		plotSpec = spec;
		// TODO: handle switching the plotUnits with the new spec
		rescaleNow.if{this.prRescalePlotData(refresh:true)};
	}

	plotWarp_ { |warp|
		// TODO:
		"updating warp not yet implemented".warn;
		// plotSpec.warp_(warp.asWarp);
		// update data
		// update gridlines
	}

	// called from other methods which set new plotMin/Max
	prRescalePlotData { |refresh=true|
		var data;

		dataScalar ?? {"data has not yet been set".warn; ^this};

		data = if (displayAbsoluteValue) { dataScalar.abs } { dataScalar };

		plotData = if (plotUnits == \db) {
			// dataScalar is scalar, spec is in db, so convert unmap
			plotSpec.copy.unmap(data.ampdb);
		} {
			plotSpec.copy.unmap(data);
		};

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
		gridVals = levelArray.select{ |level|
			level.inRange(this.plotMin, this.plotMax) // clip to plotMin/Max
		};

		plotGrid = plotSpec.copy.unmap(gridVals); // TODO: why is .copy required? it fails without it, bug?

		refresh.if{this.refresh};
	}

	// NOTE: spacing in radians (currently)
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
		thetaLines = thetaArray.select{ |theta|
			// clip to range of plot
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
		Pen.addAnnularWedge(view.prPlotCen, 0.001, view.prPlotRad, view.prStartAngle, view.prSweepLength);
		Pen.fill;
		Pen.pop;
	}
}

PolarPlotLayer : ValueViewLayer {
	*properties {
		^(
			show:        true,
			fill:        false,        // fill area under the plotted data
			plotColors:  [Color.blue],
			fillAlpha:   0.3,          // if fill == true, fill the plotColor with this alpha
			strokeWidth: 2,
			// NOTE: for some reason, pointRad won't work if listed after 'strokeType': name conflict with strokeType?
			pointRad:    2,            // if strokeType == \points, circles have pointRad radius. if nil, pointRad == strokeWidth
			strokeType:  \line,        // or: \points, or FloatArray.with(5, 2, 3, 2); (dashed alternating 5px, 3px lines separated by 2px)
			negPhaseColors: [0.3]
		)
	}

	stroke {
		var vZeroPos, vMaxRad, vThetas;
		vZeroPos = view.prZeroPos;
		vMaxRad = view.prPlotRad;
		vThetas = view.thetas * view.dirFlag;

		Pen.push;
		Pen.translate(view.prPlotCen.x, view.prPlotCen.y);

		// make sure a single color is held in an array
		if (p.plotColors.isKindOf(Color)) {
			p.plotColors = [p.plotColors]
		};

		view.plotData.do{ |dataset, i|
			var sType, pnts;

			Pen.push;
			// generate data point coordinates
			pnts = dataset.collect{ |val, j|
				Polar(
					val * vMaxRad,
					// wrapAt in case number of theta arrays is mismatched with rhoArrays
					// e.g. one theta array is provided, like [0,2pi], for multiple datasets
					vZeroPos + vThetas.wrapAt(i)[j]
				).asPoint;
			};

			// fill area under data points
			if (p.fill) {
				Pen.moveTo(pnts[0]);
				pnts.do{|pnt| Pen.lineTo(pnt)};
				Pen.fillColor_(p.plotColors.asArray.wrapAt(i).copy.alpha_(p.fillAlpha));
				Pen.fill
			};

			// draw lines/points/dashes
			sType = p.strokeType.asArray.wrapAt(i);
			case
			{sType == \points} { // data points as circles
				var rad, rect;
				rad = p.pointRad ?? {p.strokeWidth};
				rect = [0,0,rad*2, rad*2].asRect; // data point circle
				pnts.do{|pnt| Pen.addOval(rect.center_(pnt))};
			}
			{(sType == \line) or: (sType.isKindOf(FloatArray))} {
				// data points as a line, or dashed line
				Pen.moveTo(pnts[0]);
				pnts.do{|pnt| Pen.lineTo(pnt)};

				if(sType.isKindOf(FloatArray)) {
					Pen.lineDash_(sType)
				};
			}
			{warn(format("PolarPlotLayer (PolarView): unknow strokeType property: %\n", sType))};

			Pen.strokeColor_(p.plotColors.asArray.wrapAt(i));
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
			lonTxtWrap:   [0, 2pi],
			latTxtPos:    pi/4,  // radian angle of latitude labels, relative to zeroPos
			lonTxtOffset: 0.065, // percentage of the radius
			strokeWidth:  1,
		)
	}

	stroke {
		var rad = view.prPlotRad;

		Pen.push;

		/* lattitude lines */
		Pen.strokeColor_(p.strokeColor);
		Pen.width_(p.strokeWidth);

		if (p.showLatLines) {
			view.plotGrid.do{ |level|
				Pen.addArc(view.prPlotCen, level * rad, view.prStartAngle, view.prSweepLength);
			};
			Pen.stroke;
		};

		if (p.showLonLines) {
			Pen.push;
			Pen.translate(view.prPlotCen.x, view.prPlotCen.y);

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
			Pen.translate(view.prPlotCen.x, view.prPlotCen.y);

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

				Pen.stringCenteredIn(str, strBnds, Font("Helvetica", 12), p.latTxtColor);
			};
			Pen.pop;
		};
		Pen.pop;

		/* longitude labels */
		if(p.showLonVals) {
			var str, strVal, theta, strBnds, txtCen, thetaMod;

			Pen.push;
			Pen.translate(view.prPlotCen.x, view.prPlotCen.y);

			view.prThetaLines.do{ |theta, i|
				thetaMod = theta % 2pi;

				txtCen = Polar(rad*(1+p.lonTxtOffset), theta).asPoint;
				strVal = view.thetaLines[i].wrap(*p.lonTxtWrap).raddeg.round(p.lonTxtRound);
				str = if(strVal % 1 == 0, {strVal.asInt}, {strVal}).asString;
				strBnds = (str.bounds.asArray + [0,0,2,2]).asRect;
				strBnds = strBnds.center_(txtCen);

				Pen.stringCenteredIn(str, strBnds, Font("Helvetica", 12), p.lonTxtColor);
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
			align:       \bottomRight, // right, left, top, bottom, topRight, topLeft, bottomRight, bottomLeft
			inset:       10,           // inset between legend and view's edge
			margin:      10,            // inset margin between entries and border of the legend
			spacing:     8,            // spacing between legend entries
			layout:      \vertical,    // horizontal, vertical
			lineLength:  15,           // length of sample plot line
			lineSpacing: 6,            // spacing between sample line and text
			font:        Font("Helvetica"),
			fontSize:    14,
			labels:      ["Plot 1"],
			showBorder:  true,
			borderColor: Color.gray,
			borderWidth: 2,
		)
	}

	stroke {
		var lineCols, cursor, h_2, stx;
		var pntRad, strokeTypes, cRect;
		var cStep, numcSteps, cOff; // for strokeTupe == \points

		block { |break|
			lRect ?? {break.()}; // bail if background hasn't been calculated

			Pen.push;
			Pen.translate(view.cen.x, view.cen.y); // set 0,0 to center

			if (p.showBorder) {
				Pen.width_(p.borderWidth);
				Pen.strokeColor_(p.borderColor);
				Pen.addRect(lRect);
				Pen.stroke;
			};

			cursor = lRect.leftTop + (p.margin@p.margin);
			// translate to top left of legend background rect
			Pen.translate(cursor.x, cursor.y);
			cursor = 0@0; // reset cursor

			Pen.width = view.plots.strokeWidth;
			lineCols = view.plots.plotColors.asArray;
			strokeTypes = view.plots.strokeType.asArray;
			pntRad = view.plots.pointRad;
			h_2 = txtRects[0].height/2;
			cRect = [0,0,pntRad*2,pntRad*2].asRect;

			if (strokeTypes.any(_ == \points)) {
				cStep = cRect.width * 3;  // separation between pnts = pnt diam * 5
				numcSteps = (p.lineLength / cStep).asInt; // how many pnts on the line
				cOff = if (numcSteps > 0) {
					p.lineLength - (cStep * numcSteps) / 2;
				} {
					p.lineLength / 2
				}
			};

			Pen.push;
			nElem.do{ |i|
				Pen.strokeColor_(lineCols.wrapAt(i));

				if (strokeTypes.wrapAt(i).isKindOf(FloatArray)) {
					Pen.lineDash_(strokeTypes.wrapAt(i))
				};

				switch(p.layout,
					\horizontal, {
						// w = margin-lineLength-lineSpacing-text length-spacing-lineLength-lineSpacing-text length-margin, etc
						if (strokeTypes.wrapAt(i) == \points) {
							cursor = cursor + (cOff@h_2);
							Pen.addOval(cRect.center_(cursor));
							numcSteps.do {
								cursor = cursor + (cStep@0);
								Pen.addOval(cRect.center_(cursor));
							};
							cursor = cursor + ((cOff+p.lineSpacing)@h_2.neg);
						} { // lines or dashes
							cursor = cursor + (0@h_2);
							Pen.moveTo(cursor);
							cursor = cursor + (p.lineLength@0);
							Pen.lineTo(cursor);
							cursor = cursor + (p.lineSpacing@h_2.neg);
						};
						Pen.stroke;

						Pen.stringLeftJustIn(
							labels[i], txtRects[i].left_(cursor.x).top_(cursor.y),
							font, p.txtColor
						);
						cursor.x = cursor.x + txtRects[i].width + p.spacing;

					},
					\vertical, {
						// h = margin-txtHeight-spacing-txtHeight-margin
						stx = cursor.x;

						if (strokeTypes.wrapAt(i) == \points) {
							cursor = cursor + (cOff@h_2);
							Pen.addOval(cRect.center_(cursor));
							numcSteps.do {
								cursor = cursor + (cStep@0); // step
								Pen.addOval(cRect.center_(cursor));
							};
							cursor = cursor + ((cOff+p.lineSpacing)@h_2.neg);
						} {
							cursor = cursor + (0@h_2);
							Pen.moveTo(cursor);
							cursor = cursor + (p.lineLength@0);
							Pen.lineTo(cursor);
							cursor = cursor + (p.lineSpacing@h_2.neg);
						};
						Pen.stroke;

						Pen.stringLeftJustIn(
							labels[i], txtRects[i].left_(cursor.x).top_(cursor.y),
							font, p.txtColor
						);
						cursor.x = stx; // jump back to starting x
						cursor.y = cursor.y + txtRects[0].height + p.spacing;
					}
				);
			};
			Pen.pop;
			Pen.pop;
		};
	}

	fill {
		var sumW, sumH, spacing, margin;

		nElem = view.plotData.size;
		if(nElem > 0) {
			Pen.push;
			Pen.translate(view.cen.x, view.cen.y);

			if (p.labels.isKindOf(String)) {
				p.labels = [p.labels]
			};
			// make sure there are labels for all plots
			labels = p.labels.asArray.extend(nElem, " - ");

			font = p.font.size_(p.fontSize);
			txtRects = labels.collect(_.bounds(font));
			spacing = p.spacing;
			margin = p.margin;

			switch(p.layout,
				\horizontal, {
					// w = margin-lineLength-lineSpacing-textlength-spacing-lineLength-lineSpacing-text length-spacing, etc, - margin
					sumW = margin + (p.lineLength + p.lineSpacing + spacing * nElem) + txtRects.collect(_.width).sum - spacing + margin;
					sumH = (margin * 2) + txtRects[0].height;
				},
				\vertical, {
					// h = spacing-txtHeight-spacing-txtHeight-spacing
					sumW = margin + p.lineLength + p.lineSpacing + txtRects.collect(_.width).maxItem + margin;
					sumH = margin + (txtRects[0].height + spacing * nElem) - spacing + margin;
				}
			);

			lRect = [0,0, sumW, sumH].asRect.center_(0@0);

			switch(p.align,
				\right,  {lRect.right = view.bnds.width/2 - p.inset},
				\left,   {lRect.left = view.bnds.width/2.neg + p.inset},
				\top,    {lRect.top = view.bnds.height/2.neg + p.inset},
				\bottom, {lRect.bottom = view.bnds.height/2 - p.inset},
				\topRight, {
					lRect.top = view.bnds.height/2.neg + p.inset;
					lRect.right = view.bnds.width/2 - p.inset;
				},
				\topLeft, {
					lRect.top = view.bnds.height/2.neg + p.inset;
					lRect.left = view.bnds.width/2.neg + p.inset;
				},
				\bottomRight, {
					lRect.bottom = view.bnds.height/2 - p.inset;
					lRect.right = view.bnds.width/2 - p.inset;
				},
				\bottomLeft, {
					lRect.bottom = view.bnds.height/2 - p.inset;
					lRect.left = view.bnds.width/2.neg + p.inset;
				},
			);

			Pen.fillColor_(p.fillColor);
			Pen.fillRect(lRect);
			Pen.fill;
			Pen.pop;
		}
	}
}

PolarTitleLayer : ValueViewLayer {
	var font, txtRect, <bgRect, bndCalcd;

	*properties {
		^(
			show:        true,
			fillColor:   Color.white,
			txtColor:    Color.black,
			align:       \top,
			inset:       10,    // pixel distance inset from the top of the view
			margin:      15,    // margin around text and title border
			txt:         "plot",
			font:        Font("Helvetica"),
			fontSize:    18,
			showBorder:  true,
			borderColor: Color.gray,
			borderWidth: 2,
		)
	}

	// fill needs to be called before stroke
	stroke {
		Pen.push;
		if (p.showBorder) {
			Pen.width_(p.borderWidth);
			Pen.strokeColor_(p.borderColor);
			Pen.addRect(bgRect);
			Pen.stroke;
		};
		Pen.stringCenteredIn(p.txt, txtRect, font, p.txtColor);
		Pen.stroke;
		Pen.pop;
	}

	fill {
		if (bndCalcd.not) {
			this.calcBounds;
			bndCalcd = false
		};
		Pen.push;
		Pen.fillColor_(p.fillColor);
		Pen.fillRect(bgRect);
		Pen.fill;
		Pen.pop;
	}

	calcBounds {
		font = p.font.size_(p.fontSize);
		// String:-bounds doesn't account for newlines, so add it up
		txtRect = if (p.txt.contains("\n")) {
			var rects, w, h;
			rects = p.txt.split($\n).collect(_.bounds(font));
			w = rects.collect(_.width).maxItem + 4; // give a lil extra width
			h = rects[0].height * rects.size;
			[0,0,w,h].asRect
		} {
			p.txt.bounds(font)
		};
		bgRect = txtRect + [0, 0, p.margin, p.margin].asRect;
		bgRect = bgRect.center_(view.bnds.width/2 @ (bgRect.height/2 + p.inset));
		txtRect = txtRect.center_(bgRect.center);
		bndCalcd = true;
	}

}

/*

(
v = PolarView(bounds: Size(500, 500).asRect, specs: [[0,2pi].asSpec],
	initVals: 30.collect{rrand(-2pi,2pi)},
	direction: \ccw,
	zeroPos: \right,
	plotUnits: \scalar
).front;
v.title.txt_("Two plots: sine and cosine");
v.data_([0, 2pi], [sinPi((0,0.005 .. 1)), cosPi((0,0.005 .. 1))], \scalar); // sine window
v.plots.plotColors = 2.collect{Color.rand};
v.legend.labels_(["sine", "cosine"]);
)

// 2 plots of data
v.plots.fill = true
v.plots.fillAlpha = 0.3

v.levelGridLines_(1, 0.25, -1) // note -1 is the minimum, in the center
v.plots.strokeType = \points
v.plots.pointRad = 1
v.plots.strokeType = [\line,\points] // different stroke styles

v.plots.strokeType = [\points, FloatArray.with(*[6, 3, 2, 3])]
v.legend.lineLength = 25 // make lineLength long enough to see the dashed pattern
v.legend.align
v.legend.layout = \horizontal

// switch to a dB scaling
v.plotUnits = \db
v.plotMin_(-40)
v.levelGridLines_(0, 5, -40)


*/