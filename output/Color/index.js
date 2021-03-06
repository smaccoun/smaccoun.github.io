// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Boolean = require("../Data.Boolean");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Int = require("../Data.Int");
var Data_Int_Bits = require("../Data.Int.Bits");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_String_Regex = require("../Data.String.Regex");
var $$Math = require("../Math");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var UnclippedHue = function (x) {
    return x;
};
var RGB = (function () {
    function RGB() {

    };
    RGB.value = new RGB();
    return RGB;
})();
var HSL = (function () {
    function HSL() {

    };
    HSL.value = new HSL();
    return HSL;
})();
var LCh = (function () {
    function LCh() {

    };
    LCh.value = new LCh();
    return LCh;
})();
var Lab = (function () {
    function Lab() {

    };
    Lab.value = new Lab();
    return Lab;
})();
var HSLA = (function () {
    function HSLA(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    HSLA.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new HSLA(value0, value1, value2, value3);
                };
            };
        };
    };
    return HSLA;
})();
var modPos = function (x) {
    return function (y) {
        return $$Math.remainder($$Math.remainder(x)(y) + y)(y);
    };
};
var rgba = function (red$prime) {
    return function (green$prime) {
        return function (blue$prime) {
            return function (alpha) {
                var red = Data_Ord.clamp(Data_Ord.ordInt)(0)(255)(red$prime);
                var r = Data_Int.toNumber(red) / 255.0;
                var green = Data_Ord.clamp(Data_Ord.ordInt)(0)(255)(green$prime);
                var g = Data_Int.toNumber(green) / 255.0;
                var blue = Data_Ord.clamp(Data_Ord.ordInt)(0)(255)(blue$prime);
                var maxChroma = Data_Ord.max(Data_Ord.ordInt)(Data_Ord.max(Data_Ord.ordInt)(red)(green))(blue);
                var minChroma = Data_Ord.min(Data_Ord.ordInt)(Data_Ord.min(Data_Ord.ordInt)(red)(green))(blue);
                var chroma = maxChroma - minChroma | 0;
                var chroma$prime = Data_Int.toNumber(chroma) / 255.0;
                var lightness = Data_Int.toNumber(maxChroma + minChroma | 0) / (255.0 * 2.0);
                var saturation = (function () {
                    if (chroma === 0) {
                        return 0.0;
                    };
                    if (Data_Boolean.otherwise) {
                        return chroma$prime / (1.0 - $$Math.abs(2.0 * lightness - 1.0));
                    };
                    throw new Error("Failed pattern match at Color line 157, column 5 - line 158, column 75: " + [  ]);
                })();
                var b = Data_Int.toNumber(blue) / 255.0;
                var hue$prime = function (v) {
                    if (v === 0) {
                        return 0.0;
                    };
                    if (maxChroma === red) {
                        return modPos((g - b) / chroma$prime)(6.0);
                    };
                    if (maxChroma === green) {
                        return (b - r) / chroma$prime + 2.0;
                    };
                    if (Data_Boolean.otherwise) {
                        return (r - g) / chroma$prime + 4.0;
                    };
                    throw new Error("Failed pattern match at Color line 148, column 5 - line 149, column 5: " + [ v.constructor.name ]);
                };
                var hue = 60.0 * hue$prime(chroma);
                return new HSLA(hue, saturation, lightness, alpha);
            };
        };
    };
};
var rgb = function (r) {
    return function (g) {
        return function (b) {
            return rgba(r)(g)(b)(1.0);
        };
    };
};
var rgba$prime = function (r) {
    return function (g) {
        return function (b) {
            return function (a) {
                return rgba(Data_Int.round(r * 255.0))(Data_Int.round(g * 255.0))(Data_Int.round(b * 255.0))(a);
            };
        };
    };
};
var rgb$prime = function (r) {
    return function (g) {
        return function (b) {
            return rgba$prime(r)(g)(b)(1.0);
        };
    };
};
var xyz = function (x) {
    return function (y) {
        return function (z) {
            var f = function (c) {
                if (c <= 3.1308e-3) {
                    return 12.92 * c;
                };
                if (Data_Boolean.otherwise) {
                    return 1.055 * $$Math.pow(c)(1.0 / 2.4) - 5.5e-2;
                };
                throw new Error("Failed pattern match at Color line 224, column 5 - line 227, column 1: " + [ c.constructor.name ]);
            };
            var g = f(-0.9689 * x + 1.8758 * y + 4.15e-2 * z);
            var r = f(3.2406 * x - 1.5372 * y - 0.4986 * z);
            var b = f((5.57e-2 * x - 0.204 * y) + 1.057 * z);
            return rgb$prime(r)(g)(b);
        };
    };
};
var interpolate = function (fraction) {
    return function (a) {
        return function (b) {
            return a + fraction * (b - a);
        };
    };
};
var interpolateAngle = function (fraction) {
    return function (a) {
        return function (b) {
            var paths = [ {
                from: a,
                to: b
            }, {
                from: a,
                to: b + 360.0
            }, {
                from: a + 360.0,
                to: b
            } ];
            var dist = function (v) {
                return $$Math.abs(v.to - v.from);
            };
            var shortest = Data_Maybe.fromJust()(Data_Foldable.minimumBy(Data_Foldable.foldableArray)(Data_Ord.comparing(Data_Ord.ordNumber)(dist))(paths));
            return interpolate(fraction)(shortest.from)(shortest.to);
        };
    };
};
var mixCubehelix = function (gamma) {
    return function (v) {
        return function (v1) {
            var radians = $$Math.pi / 180.0;
            var bs = v1.value1 - v.value1;
            var bl = v1.value2 - v.value2;
            var ah = (v.value0 + 120.0) * radians;
            var bh = (v1.value0 + 120.0) * radians - ah;
            return function (t) {
                var fract = $$Math.pow(v.value2 + bl * t)(gamma);
                var angle = ah + bh * t;
                var amp = (v.value1 + bs * t) * fract * (1.0 - fract);
                var b = fract + amp * (1.97294 * $$Math.cos(angle));
                var g = fract + amp * (-0.29227 * $$Math.cos(angle) - 0.90649 * $$Math.sin(angle));
                var r = fract + amp * (-0.14861 * $$Math.cos(angle) + 1.78277 * $$Math.sin(angle));
                var a = interpolate(t)(v.value3)(v1.value3);
                return rgba$prime(r)(g)(b)(a);
            };
        };
    };
};
var hsla = function (h) {
    return function (s) {
        return function (l) {
            return function (a) {
                var s$prime = Data_Ord.clamp(Data_Ord.ordNumber)(0.0)(1.0)(s);
                var l$prime = Data_Ord.clamp(Data_Ord.ordNumber)(0.0)(1.0)(l);
                var a$prime = Data_Ord.clamp(Data_Ord.ordNumber)(0.0)(1.0)(a);
                return new HSLA(h, s$prime, l$prime, a$prime);
            };
        };
    };
};
var hsva = function (h) {
    return function (v) {
        return function (v1) {
            return function (a) {
                var s = v;
                if (v1 === 0.0) {
                    return hsla(h)(s / (2.0 - s))(0.0)(a);
                };
                if (v === 0.0 && v1 === 1.0) {
                    return hsla(h)(0.0)(1.0)(a);
                };
                var tmp = (2.0 - v) * v1;
                var s = (v * v1) / (function () {
                    var $41 = tmp < 1.0;
                    if ($41) {
                        return tmp;
                    };
                    return 2.0 - tmp;
                })();
                var l = tmp / 2.0;
                return hsla(h)(s)(l)(a);
            };
        };
    };
};
var hsv = function (h) {
    return function (s) {
        return function (v) {
            return hsva(h)(s)(v)(1.0);
        };
    };
};
var lighten = function (f) {
    return function (v) {
        return hsla(v.value0)(v.value1)(v.value2 + f)(v.value3);
    };
};
var rotateHue = function (angle) {
    return function (v) {
        return hsla(v.value0 + angle)(v.value1)(v.value2)(v.value3);
    };
};
var saturate = function (f) {
    return function (v) {
        return hsla(v.value0)(v.value1 + f)(v.value2)(v.value3);
    };
};
var hsl = function (h) {
    return function (s) {
        return function (l) {
            return hsla(h)(s)(l)(1.0);
        };
    };
};
var white = hsl(0.0)(0.0)(1.0);
var graytone = function (l) {
    return hsl(0.0)(0.0)(l);
};
var fromInt = function (m) {
    var n = Data_Ord.clamp(Data_Ord.ordInt)(0)(16777215)(m);
    var r = n >> 16 & 255;
    var g = n >> 8 & 255;
    var b = n & 255;
    return rgb(r)(g)(b);
};
var fromHexString = function (str) {
    var parseHex = function ($111) {
        return Data_Maybe.fromMaybe(0)(Data_Int.fromStringAs(Data_Int.hexadecimal)($111));
    };
    var isShort = Data_String.length(str) === 4;
    var hush = Data_Either.either(Data_Function["const"](Data_Maybe.Nothing.value))(Data_Maybe.Just.create);
    var pair = "(" + ("[0-9a-f]" + ("[0-9a-f]" + ")"));
    var single = "(" + ("[0-9a-f]" + ")");
    var variant = (function () {
        if (isShort) {
            return single + (single + single);
        };
        return pair + (pair + pair);
    })();
    var mPattern = Data_String_Regex.regex("^#(?:" + (variant + ")$"))(Data_String_Regex.parseFlags("i"));
    return Control_Bind.bind(Data_Maybe.bindMaybe)(hush(mPattern))(function (v) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_String_Regex.match(v)(str))(function (v1) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(parseHex)(Control_Bind.join(Data_Maybe.bindMaybe)(Data_Array.index(v1)(1))))(function (v2) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(parseHex)(Control_Bind.join(Data_Maybe.bindMaybe)(Data_Array.index(v1)(2))))(function (v3) {
                    return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(parseHex)(Control_Bind.join(Data_Maybe.bindMaybe)(Data_Array.index(v1)(3))))(function (v4) {
                        if (isShort) {
                            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(rgb((16 * v2 | 0) + v2 | 0)((16 * v3 | 0) + v3 | 0)((16 * v4 | 0) + v4 | 0));
                        };
                        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(rgb(v2)(v3)(v4));
                    });
                });
            });
        });
    });
};
var desaturate = function (f) {
    return saturate(-f);
};
var darken = function (f) {
    return lighten(-f);
};
var d65 = {
    xn: 0.95047,
    yn: 1.0,
    zn: 1.08883
};
var lab = function (l) {
    return function (a) {
        return function (b) {
            var l$prime = (l + 16.0) / 116.0;
            var delta = 6.0 / 29.0;
            var finv = function (t) {
                if (t > delta) {
                    return $$Math.pow(t)(3.0);
                };
                if (Data_Boolean.otherwise) {
                    return 3.0 * delta * delta * (t - 4.0 / 29.0);
                };
                throw new Error("Failed pattern match at Color line 249, column 5 - line 250, column 64: " + [ t.constructor.name ]);
            };
            var x = d65.xn * finv(l$prime + a / 500.0);
            var y = d65.yn * finv(l$prime);
            var z = d65.zn * finv(l$prime - b / 200.0);
            return xyz(x)(y)(z);
        };
    };
};
var lch = function (l) {
    return function (c) {
        return function (h) {
            var deg2rad = $$Math.pi / 180.0;
            var b = c * $$Math.sin(h * deg2rad);
            var a = c * $$Math.cos(h * deg2rad);
            return lab(l)(a)(b);
        };
    };
};
var cssStringHSLA = function (v) {
    var toString = function (n) {
        return Data_Show.show(Data_Show.showNumber)(Data_Int.toNumber(Data_Int.round(100.0 * n)) / 100.0);
    };
    var saturation = toString(v.value1 * 100.0) + "%";
    var lightness = toString(v.value2 * 100.0) + "%";
    var hue = toString(v.value0);
    var alpha = Data_Show.show(Data_Show.showNumber)(v.value3);
    var $69 = v.value3 === 1.0;
    if ($69) {
        return "hsl(" + (hue + (", " + (saturation + (", " + (lightness + ")")))));
    };
    return "hsla(" + (hue + (", " + (saturation + (", " + (lightness + (", " + (alpha + ")")))))));
};
var complementary = rotateHue(180.0);
var clipHue = function (v) {
    var $75 = 360.0 === v;
    if ($75) {
        return v;
    };
    return modPos(v)(360.0);
};
var toHSLA = function (v) {
    return {
        h: clipHue(v.value0),
        s: v.value1,
        l: v.value2,
        a: v.value3
    };
};
var toHSVA = function (v) {
    var s = v.value1;
    if (v.value2 === 0.0) {
        return {
            h: clipHue(v.value0),
            s: (2.0 * s) / (1.0 + s),
            v: 0.0,
            a: v.value3
        };
    };
    if (v.value1 === 0.0 && v.value2 === 1.0) {
        return {
            h: clipHue(v.value0),
            s: 0.0,
            v: 1.0,
            a: v.value3
        };
    };
    var tmp = v.value1 * (function () {
        var $90 = v.value2 < 0.5;
        if ($90) {
            return v.value2;
        };
        return 1.0 - v.value2;
    })();
    var v1 = v.value2 + tmp;
    var s = (2.0 * tmp) / (v.value2 + tmp);
    return {
        h: clipHue(v.value0),
        s: s,
        v: v1,
        a: v.value3
    };
};
var toRGBA$prime = function (v) {
    var h$prime = clipHue(v.value0) / 60.0;
    var chr = (1.0 - $$Math.abs(2.0 * v.value2 - 1.0)) * v.value1;
    var m = v.value2 - chr / 2.0;
    var x = chr * (1.0 - $$Math.abs($$Math.remainder(h$prime)(2.0) - 1.0));
    var col = (function () {
        if (h$prime < 1.0) {
            return {
                r: chr,
                g: x,
                b: 0.0
            };
        };
        if (1.0 <= h$prime && h$prime < 2.0) {
            return {
                r: x,
                g: chr,
                b: 0.0
            };
        };
        if (2.0 <= h$prime && h$prime < 3.0) {
            return {
                r: 0.0,
                g: chr,
                b: x
            };
        };
        if (3.0 <= h$prime && h$prime < 4.0) {
            return {
                r: 0.0,
                g: x,
                b: chr
            };
        };
        if (4.0 <= h$prime && h$prime < 5.0) {
            return {
                r: x,
                g: 0.0,
                b: chr
            };
        };
        if (Data_Boolean.otherwise) {
            return {
                r: chr,
                g: 0.0,
                b: x
            };
        };
        throw new Error("Failed pattern match at Color line 342, column 5 - line 347, column 61: " + [  ]);
    })();
    return {
        r: col.r + m,
        g: col.g + m,
        b: col.b + m,
        a: v.value3
    };
};
var luminance = function (col) {
    var val = toRGBA$prime(col);
    var f = function (c) {
        if (c <= 3.928e-2) {
            return c / 12.92;
        };
        if (Data_Boolean.otherwise) {
            return $$Math.pow((c + 5.5e-2) / 1.055)(2.4);
        };
        throw new Error("Failed pattern match at Color line 604, column 9 - line 607, column 9: " + [ c.constructor.name ]);
    };
    var g = f(val.g);
    var r = f(val.r);
    var b = f(val.b);
    return 0.2126 * r + 0.7152 * g + 7.22e-2 * b;
};
var contrast = function (c1) {
    return function (c2) {
        var l2 = luminance(c2);
        var l1 = luminance(c1);
        var $101 = l1 > l2;
        if ($101) {
            return (l1 + 5.0e-2) / (l2 + 5.0e-2);
        };
        return (l2 + 5.0e-2) / (l1 + 5.0e-2);
    };
};
var isReadable = function (c1) {
    return function (c2) {
        return contrast(c1)(c2) > 4.5;
    };
};
var toRGBA = function (col) {
    var c = toRGBA$prime(col);
    var g = Data_Int.round(255.0 * c.g);
    var r = Data_Int.round(255.0 * c.r);
    var b = Data_Int.round(255.0 * c.b);
    return {
        r: r,
        g: g,
        b: b,
        a: c.a
    };
};
var cssStringRGBA = function (col) {
    var c = toRGBA(col);
    var green = Data_Show.show(Data_Show.showInt)(c.g);
    var red = Data_Show.show(Data_Show.showInt)(c.r);
    var blue = Data_Show.show(Data_Show.showInt)(c.b);
    var alpha = Data_Show.show(Data_Show.showNumber)(c.a);
    var $102 = c.a === 1.0;
    if ($102) {
        return "rgb(" + (red + (", " + (green + (", " + (blue + ")")))));
    };
    return "rgba(" + (red + (", " + (green + (", " + (blue + (", " + (alpha + ")")))))));
};
var eqColor = new Data_Eq.Eq(function (c1) {
    return function (c2) {
        var rgb2 = toRGBA(c2);
        var rgb1 = toRGBA(c1);
        return rgb1.r === rgb2.r && (rgb1.g === rgb2.g && (rgb1.b === rgb2.b && rgb1.a === rgb2.a));
    };
});
var showColor = new Data_Show.Show(function (c) {
    var col = toRGBA(c);
    return "rgba " + (Data_Show.show(Data_Show.showInt)(col.r) + (" " + (Data_Show.show(Data_Show.showInt)(col.g) + (" " + (Data_Show.show(Data_Show.showInt)(col.b) + (" " + Data_Show.show(Data_Show.showNumber)(col.a)))))));
});
var toHexString = function (color) {
    var toHex = function (num) {
        var repr = Data_Int.toStringAs(Data_Int.hexadecimal)(num);
        var $103 = Data_String.length(repr) === 1;
        if ($103) {
            return "0" + repr;
        };
        return repr;
    };
    var c = toRGBA(color);
    return "#" + (toHex(c.r) + (toHex(c.g) + toHex(c.b)));
};
var toXYZ = function (c) {
    var rec = toRGBA$prime(c);
    var finv = function (c$prime) {
        if (c$prime <= 4.045e-2) {
            return c$prime / 12.92;
        };
        if (Data_Boolean.otherwise) {
            return $$Math.pow((c$prime + 5.5e-2) / 1.055)(2.4);
        };
        throw new Error("Failed pattern match at Color line 366, column 5 - line 369, column 1: " + [ c$prime.constructor.name ]);
    };
    var g = finv(rec.g);
    var r = finv(rec.r);
    var b = finv(rec.b);
    var x = 0.4124 * r + 0.3576 * g + 0.1805 * b;
    var y = 0.2126 * r + 0.7152 * g + 7.22e-2 * b;
    var z = 1.93e-2 * r + 0.1192 * g + 0.9505 * b;
    return {
        x: x,
        y: y,
        z: z
    };
};
var toLab = function (col) {
    var rec = toXYZ(col);
    var cut = $$Math.pow(6.0 / 29.0)(3.0);
    var f = function (t) {
        if (t > cut) {
            return $$Math.pow(t)(1.0 / 3.0);
        };
        if (Data_Boolean.otherwise) {
            return (1.0 / 3.0) * $$Math.pow(29.0 / 6.0)(2.0) * t + 4.0 / 29.0;
        };
        throw new Error("Failed pattern match at Color line 384, column 5 - line 387, column 1: " + [ t.constructor.name ]);
    };
    var fy = f(rec.y / d65.yn);
    var l = 116.0 * fy - 16.0;
    var b = 200.0 * (fy - f(rec.z / d65.zn));
    var a = 500.0 * (f(rec.x / d65.xn) - fy);
    return {
        l: l,
        a: a,
        b: b
    };
};
var distance = function (col1) {
    return function (col2) {
        var sq = function (x) {
            return $$Math.pow(x)(2.0);
        };
        var c2 = toLab(col2);
        var c1 = toLab(col1);
        return $$Math.sqrt(sq(c1.l - c2.l) + sq(c1.a - c2.a) + sq(c1.b - c2.b));
    };
};
var toLCh = function (col) {
    var rec = toLab(col);
    var rad2deg = 180.0 / $$Math.pi;
    var c = $$Math.sqrt(rec.a * rec.a + rec.b * rec.b);
    var h = modPos($$Math.atan2(rec.b)(rec.a) * rad2deg)(360.0);
    return {
        l: rec.l,
        c: c,
        h: h
    };
};
var mix = function (v) {
    return function (c1) {
        return function (c2) {
            return function (frac) {
                if (v instanceof HSL) {
                    var t = toHSLA(c2);
                    var f = toHSLA(c1);
                    return hsla(interpolateAngle(frac)(f.h)(t.h))(interpolate(frac)(f.s)(t.s))(interpolate(frac)(f.l)(t.l))(interpolate(frac)(f.a)(t.a));
                };
                if (v instanceof RGB) {
                    var t = toRGBA$prime(c2);
                    var f = toRGBA$prime(c1);
                    return rgba$prime(interpolate(frac)(f.r)(t.r))(interpolate(frac)(f.g)(t.g))(interpolate(frac)(f.b)(t.b))(interpolate(frac)(f.a)(t.a));
                };
                if (v instanceof LCh) {
                    var t = toLCh(c2);
                    var f = toLCh(c1);
                    return lch(interpolate(frac)(f.l)(t.l))(interpolate(frac)(f.c)(t.c))(interpolateAngle(frac)(f.h)(t.h));
                };
                if (v instanceof Lab) {
                    var t = toLab(c2);
                    var f = toLab(c1);
                    return lab(interpolate(frac)(f.l)(t.l))(interpolate(frac)(f.a)(t.a))(interpolate(frac)(f.b)(t.b));
                };
                throw new Error("Failed pattern match at Color line 520, column 1 - line 520, column 34: " + [ v.constructor.name, c1.constructor.name, c2.constructor.name, frac.constructor.name ]);
            };
        };
    };
};
var toGray = function (col) {
    var res = toLCh(col);
    return desaturate(1.0)(lch(res.l)(0.0)(0.0));
};
var brightness = function (col) {
    var c = toRGBA$prime(col);
    return (299.0 * c.r + 587.0 * c.g + 114.0 * c.b) / 1000.0;
};
var isLight = function (c) {
    return brightness(c) > 0.5;
};
var black = hsl(0.0)(0.0)(0.0);
var textColor = function (c) {
    if (isLight(c)) {
        return black;
    };
    if (Data_Boolean.otherwise) {
        return white;
    };
    throw new Error("Failed pattern match at Color line 643, column 1 - line 643, column 28: " + [ c.constructor.name ]);
};
module.exports = {
    RGB: RGB,
    HSL: HSL,
    LCh: LCh,
    Lab: Lab,
    rgba: rgba,
    rgb: rgb,
    "rgba'": rgba$prime,
    "rgb'": rgb$prime,
    hsla: hsla,
    hsl: hsl,
    hsva: hsva,
    hsv: hsv,
    xyz: xyz,
    lab: lab,
    lch: lch,
    fromHexString: fromHexString,
    fromInt: fromInt,
    toHSLA: toHSLA,
    toHSVA: toHSVA,
    toRGBA: toRGBA,
    "toRGBA'": toRGBA$prime,
    toXYZ: toXYZ,
    toLab: toLab,
    toLCh: toLCh,
    toHexString: toHexString,
    cssStringHSLA: cssStringHSLA,
    cssStringRGBA: cssStringRGBA,
    black: black,
    white: white,
    graytone: graytone,
    rotateHue: rotateHue,
    complementary: complementary,
    lighten: lighten,
    darken: darken,
    saturate: saturate,
    desaturate: desaturate,
    toGray: toGray,
    mix: mix,
    mixCubehelix: mixCubehelix,
    brightness: brightness,
    luminance: luminance,
    contrast: contrast,
    isLight: isLight,
    isReadable: isReadable,
    textColor: textColor,
    distance: distance,
    showColor: showColor,
    eqColor: eqColor
};
