<?php
/**
 * Smarty PHPunit tests - issue #549 regression tests
 *

 * @author  Andrey Repin <anrdaemon@yandex.ru>
 */

/**
 * class for compiler tests
 *
 * 
 * @preserveGlobalState    disabled
 * 
 *
 * replace modifier must work regardless of Smarty encoding.
 */
class MbSplitEncodingIssue549Test extends PHPUnit_Smarty
{
    /** @var string Saved Smarty charset */
    private $charset;

    /** @var array Source data for tests, hexed to protect from accidental reencoding */
    private $data = array(
        "subject" => '4772c3bc6e6577616c64', // "Grünewald"
        "pattern" => '77616c64', // "wald"
        "replacement" => '7374c3bc726d', // "stürm"
        "result" => '4772c3bc6e657374c3bc726d', // "Grünestürm"
    );

	public function setUp(): void
    {
        $this->charset = \Smarty\Smarty::$_CHARSET;
        $this->setUpSmarty(__DIR__);
    }

    protected function tearDown(): void
    {
        \Smarty\Smarty::$_CHARSET = $this->charset ?: \Smarty\Smarty::$_CHARSET;
        $this->cleanDirs();
    }

    /** Provider for testReplaceModifier
     */
    public function encodingProvider()
    {
        return array(
            "with non-UNICODE src" => array("Windows-1252"),
            "with UTF-8 src" => array("UTF-8"),
        );
    }

    /** Test behavior of `replace` modifier with different source encodings
     *
     * @dataProvider encodingProvider
     */
    public function testReplaceModifier($mb_int_encoding)
    {
        $data = $this->data;
        \array_walk($data, function(&$value, $key) use($mb_int_encoding) {
            $value = \mb_convert_encoding(pack("H*", $value), $mb_int_encoding, "UTF-8");
        });
        \extract($data, \EXTR_SKIP);

        \Smarty\Smarty::$_CHARSET = $mb_int_encoding;
        $this->assertEquals($result, $this->smarty->fetch("string:{\"$subject\"|replace:\"$pattern\":\"$replacement\"}"));
    }

    /** Test that `replace` modifier does not match inside a multibyte character
     *
     * In Shift_JIS, the second byte of "ソ" (0x83 0x5C) is the same as a backslash (0x5C).
     */
    public function testReplaceModifierDoesNotMatchInsideMultibyteCharacter()
    {
        \Smarty\Smarty::$_CHARSET = "SJIS";
        $this->smarty->assign("subject", pack("H*", "835c"));
        $this->assertEquals(pack("H*", "835c"), $this->smarty->fetch("string:{\$subject|replace:\"\\\\\":\"x\"}"));
    }

    /** Provider for testReplaceModifierWithInvalidUtf8
     */
    public function invalidUtf8Provider()
    {
        return array(
            "with invalid subject" => array("61ff61", "61"),
            "with invalid search" => array("616263", "ff"),
        );
    }

    /** Test that `replace` modifier returns an empty string for invalid UTF-8 input
     *
     * This keeps the result of the former mb_split() based implementation.
     *
     * @dataProvider invalidUtf8Provider
     */
    public function testReplaceModifierWithInvalidUtf8($subject, $search)
    {
        \Smarty\Smarty::$_CHARSET = "UTF-8";
        $this->smarty->assign("subject", pack("H*", $subject));
        $this->smarty->assign("search", pack("H*", $search));
        $this->assertEquals("", $this->smarty->fetch("string:{\$subject|replace:\$search:\"x\"}"));
    }

}
