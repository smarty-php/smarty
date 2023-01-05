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
 * mb_split breaks if Smarty encoding is not the same as mbstring regex encoding.
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
    public function encodingPairsProvider()
    {
        return array(
            "with non-UNICODE src/non-UNICODE regex (PHP < 5.6 default)" => array("Windows-1252", "EUC-JP"),
            "with UTF-8 src/non-UNICODE regex (PHP < 5.6 default)" => array("UTF-8", "EUC-JP"),
            "with UTF-8 src/UTF-8 regex (PHP >= 5.6)" => array("UTF-8", "UTF-8"),
            "with non-UNICODE src/UTF-8 regex" => array("Windows-1252", "UTF-8"),
        );
    }

    /** Test behavior of `replace` modifier with different source and regex encodings
     *
     * @dataProvider encodingPairsProvider
     */
    public function testReplaceModifier($mb_int_encoding, $mb_regex_encoding)
    {
        $data = $this->data;
        \array_walk($data, function(&$value, $key) use($mb_int_encoding) {
            $value = \mb_convert_encoding(pack("H*", $value), $mb_int_encoding, "UTF-8");
        });
        \extract($data, \EXTR_SKIP);

        \mb_regex_encoding($mb_regex_encoding);
        \Smarty\Smarty::$_CHARSET = $mb_int_encoding;
        $this->assertEquals($result, $this->smarty->fetch("string:{\"$subject\"|replace:\"$pattern\":\"$replacement\"}"));
    }

}
