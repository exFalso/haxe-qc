#if js
import js.Node;
#end

class InOut
{

	public static function readStdin(callback:String->Void):Void
	{
#if js
		Node.process.stdin.setEncoding("utf8");
		var str = "";
		function readStdinChunk():Null<String> {
			return untyped __js__("process.stdin.read()");
		}
		Node.process.stdin.on('readable', function() {
				var chunk = readStdinChunk();
				if (chunk != null) {
					str += chunk;
				}
			});
		Node.process.stdin.on('end', function() {
				callback(str);
			});
#end
#if java
		callback(Sys.stdin().readAll().toString());
#end
	}

	public static function writeStdout(str:String)
	{
#if js
		Node.process.stdout.write(str);
#end
#if java
		Sys.stdout().writeString(str);
		Sys.stdout().flush();
#end

	}

	public static function run(fun:String->String)
	{
		readStdin(function(stdin) {
			writeStdout(fun(stdin));
		});
	}
}
