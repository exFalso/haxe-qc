package com.prezi.haxe;

public class StringTools {

    private static RuntimeException uriError() {
        return new RuntimeException("URIError: msg.bad.uri");
    }

    // See https://github.com/mozilla/rhino/blob/4331f961a88fc70e28b852936f563baf8bf4d3a2/src/org/mozilla/javascript/NativeGlobal.java#L615
    public static String urlDecode(String str) {
        char[] buf = null;
        int bufTop = 0;
        boolean fullUri = false;

        for (int k = 0, length = str.length(); k != length;) {
            char C = str.charAt(k);
            if (C != '%') {
                if (buf != null) {
                    buf[bufTop++] = C;
                }
                ++k;
            } else {
                if (buf == null) {
                    // decode always compress so result can not be bigger then
                    // str.length()
                    buf = new char[length];
                    str.getChars(0, k, buf, 0);
                    bufTop = k;
                }
                int start = k;
                if (k + 3 > length)
                    throw uriError();
                int B = unHex(str.charAt(k + 1), str.charAt(k + 2));
                if (B < 0) throw uriError();
                k += 3;
                if ((B & 0x80) == 0) {
                    C = (char)B;
                } else {
                    // Decode UTF-8 sequence into ucs4Char and encode it into
                    // UTF-16
                    int utf8Tail, ucs4Char, minUcs4Char;
                    if ((B & 0xC0) == 0x80) {
                        // First  UTF-8 should be ouside 0x80..0xBF
                        throw uriError();
                    } else if ((B & 0x20) == 0) {
                        utf8Tail = 1; ucs4Char = B & 0x1F;
                        minUcs4Char = 0x80;
                    } else if ((B & 0x10) == 0) {
                        utf8Tail = 2; ucs4Char = B & 0x0F;
                        minUcs4Char = 0x800;
                    } else if ((B & 0x08) == 0) {
                        utf8Tail = 3; ucs4Char = B & 0x07;
                        minUcs4Char = 0x10000;
                    } else if ((B & 0x04) == 0) {
                        utf8Tail = 4; ucs4Char = B & 0x03;
                        minUcs4Char = 0x200000;
                    } else if ((B & 0x02) == 0) {
                        utf8Tail = 5; ucs4Char = B & 0x01;
                        minUcs4Char = 0x4000000;
                    } else {
                        // First UTF-8 can not be 0xFF or 0xFE
                        throw uriError();
                    }
                    if (k + 3 * utf8Tail > length)
                        throw uriError();
                    for (int j = 0; j != utf8Tail; j++) {
                        if (str.charAt(k) != '%')
                            throw uriError();
                        B = unHex(str.charAt(k + 1), str.charAt(k + 2));
                        if (B < 0 || (B & 0xC0) != 0x80)
                            throw uriError();
                        ucs4Char = (ucs4Char << 6) | (B & 0x3F);
                        k += 3;
                    }
                    // Check for overlongs and other should-not-present codes
                    if (ucs4Char < minUcs4Char
                            || (ucs4Char >= 0xD800 && ucs4Char <= 0xDFFF)) {
                        ucs4Char = INVALID_UTF8;
                    } else if (ucs4Char == 0xFFFE || ucs4Char == 0xFFFF) {
                        ucs4Char = 0xFFFD;
                    }
                    if (ucs4Char >= 0x10000) {
                        ucs4Char -= 0x10000;
                        if (ucs4Char > 0xFFFFF) {
                            throw uriError();
                        }
                        char H = (char)((ucs4Char >>> 10) + 0xD800);
                        C = (char)((ucs4Char & 0x3FF) + 0xDC00);
                        buf[bufTop++] = H;
                    } else {
                        C = (char)ucs4Char;
                    }
                }
                if (fullUri && URI_DECODE_RESERVED.indexOf(C) >= 0) {
                    for (int x = start; x != k; x++) {
                        buf[bufTop++] = str.charAt(x);
                    }
                } else {
                    buf[bufTop++] = C;
                }
            }
        }
        return (buf == null) ? str : new String(buf, 0, bufTop);
    }

    public static String urlEncode(String str) {
        boolean fullUri = false;
        byte[] utf8buf = null;
        StringBuilder sb = null;

        for (int k = 0, length = str.length(); k != length; ++k) {
            char C = str.charAt(k);
            if (encodeUnescaped(C, fullUri)) {
                if (sb != null) {
                    sb.append(C);
                }
            } else {
                if (sb == null) {
                    sb = new StringBuilder(length + 3);
                    sb.append(str);
                    sb.setLength(k);
                    utf8buf = new byte[6];
                }
                if (0xDC00 <= C && C <= 0xDFFF) {
                    throw uriError();
                }
                int V;
                if (C < 0xD800 || 0xDBFF < C) {
                    V = C;
                } else {
                    k++;
                    if (k == length) {
                        throw uriError();
                    }
                    char C2 = str.charAt(k);
                    if (!(0xDC00 <= C2 && C2 <= 0xDFFF)) {
                        throw uriError();
                    }
                    V = ((C - 0xD800) << 10) + (C2 - 0xDC00) + 0x10000;
                }
                int L = oneUcs4ToUtf8Char(utf8buf, V);
                for (int j = 0; j < L; j++) {
                    int d = 0xff & utf8buf[j];
                    sb.append('%');
                    sb.append(toHexChar(d >>> 4));
                    sb.append(toHexChar(d & 0xf));
                }
            }
        }
        return (sb == null) ? str : sb.toString();
    }

    private static int unHex(char c) {
        if ('A' <= c && c <= 'F') {
            return c - 'A' + 10;
        } else if ('a' <= c && c <= 'f') {
            return c - 'a' + 10;
        } else if ('0' <= c && c <= '9') {
            return c - '0';
        } else {
            return -1;
        }
    }

    private static int unHex(char c1, char c2) {
        int i1 = unHex(c1);
        int i2 = unHex(c2);
        if (i1 >= 0 && i2 >= 0) {
            return (i1 << 4) | i2;
        }
        return -1;
    }

    private static boolean encodeUnescaped(char c, boolean fullUri) {
        if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
                || ('0' <= c && c <= '9')) {
            return true;
        }
        if ("-_.!~*'()".indexOf(c) >= 0) {
            return true;
        }
        if (fullUri) {
            return URI_DECODE_RESERVED.indexOf(c) >= 0;
        }
        return false;
    }

    private static int oneUcs4ToUtf8Char(byte[] utf8Buffer, int ucs4Char) {
        int utf8Length = 1;

        //JS_ASSERT(ucs4Char <= 0x7FFFFFFF);
        if ((ucs4Char & ~0x7F) == 0)
            utf8Buffer[0] = (byte)ucs4Char;
        else {
            int i;
            int a = ucs4Char >>> 11;
            utf8Length = 2;
            while (a != 0) {
                a >>>= 5;
                utf8Length++;
            }
            i = utf8Length;
            while (--i > 0) {
                utf8Buffer[i] = (byte)((ucs4Char & 0x3F) | 0x80);
                ucs4Char >>>= 6;
            }
            utf8Buffer[0] = (byte)(0x100 - (1 << (8-utf8Length)) + ucs4Char);
        }
        return utf8Length;
    }

    private static char toHexChar(int i) {
        if (i >> 4 != 0) codeBug();
        return (char)((i < 10) ? i + '0' : i - 10 + 'A');
    }

    public static RuntimeException codeBug()
        throws RuntimeException
    {
        RuntimeException ex = new IllegalStateException("FAILED ASSERTION");
        // Print stack trace ASAP
        ex.printStackTrace(System.err);
        throw ex;
    }

    private static final String URI_DECODE_RESERVED = ";/?:@&=+$,#";
    private static final int INVALID_UTF8 = Integer.MAX_VALUE;
}
