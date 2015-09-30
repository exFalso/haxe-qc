/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

import java.Boot;
import java.Lib;
import java.internal.Exceptions;

@:coreApi @:nativeGen class Std {
	public static function is( v : Dynamic, t : Dynamic ) : Bool
	{
		if (v == null)
			return t == Dynamic;
		if (t == null)
			return false;
		var clt:java.lang.Class<Dynamic> = cast t;
		if (clt == null)
			return false;
		var name:String = clt.getName();

		switch(name)
		{
			case "double", "java.lang.Double":
				return untyped __java__('haxe.lang.Runtime.isDouble(v)');
			case "int", "java.lang.Integer":
				return untyped __java__('haxe.lang.Runtime.isInt(v)');
			case "boolean", "java.lang.Boolean":
				return untyped __java__('v instanceof java.lang.Boolean');
			case "java.lang.Object":
				return true;
		}

		var clv:java.lang.Class<Dynamic> = untyped __java__('v.getClass()');

		return clt.isAssignableFrom(clv);
	}

	public static function string( s : Dynamic ) : String {
		return cast(s, String) + "";
	}

	public static inline function int( x : Float ) : Int {
		return cast x;
	}

	public static function parseInt( x : String ) : Null<Int> {
		var v:Dynamic = untyped __java__("com.prezi.haxe.Std.parseInt(x, 10)");
		if( v == 0 && (x.charCodeAt(1) == 'x'.code || x.charCodeAt(1) == 'X'.code) )
			v = untyped __java__("com.prezi.haxe.Std.parseInt(x, null)");
		if (v != v)
			return null;
		return cast v;
	}

	public static function parseFloat( x : String ) : Float {
		return untyped __java__("com.prezi.haxe.Std.parseFloat(x)");
	}

	inline public static function instance<T:{},S:T>( value : T, c : Class<S> ) : S {
		return Std.is(value, c) ? cast value : null;
	}

	public static function random( x : Int ) : Int {
		if (x <= 0) return 0;
		return Std.int(Math.random() * x);
	}

}