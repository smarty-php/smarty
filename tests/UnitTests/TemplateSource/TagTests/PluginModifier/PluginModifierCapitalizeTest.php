<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginModifierCapitalizeTest extends PHPUnit_Smarty
{
     public function setUp()
    {
        $this->setUpSmarty(__DIR__);
        error_reporting(E_ALL  & ~E_DEPRECATED | E_STRICT);
    }

    public function testDefault()
    {
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, delayed. ümlauts äre cööl."|capitalize}');
        $this->assertEquals("Next X-Men FiLm, x3, Delayed. Ümlauts Äre Cööl.", $this->smarty->fetch($tpl));
    }

    public function testDigits()
    {
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, delayed. ümlauts äre cööl."|capitalize:true}');
        $this->assertEquals("Next X-Men FiLm, X3, Delayed. Ümlauts Äre Cööl.", $this->smarty->fetch($tpl));
    }

    public function testTrueCaptials()
    {
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, delayed. ümlauts äre cööl."|capitalize:true:true}');
        $this->assertEquals("Next X-Men Film, X3, Delayed. Ümlauts Äre Cööl.", $this->smarty->fetch($tpl));
    }

    public function testDefaultWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, delayed."|capitalize}');
        $this->assertEquals("Next X-Men FiLm, x3, Delayed.", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testDigitsWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, delayed."|capitalize:true}');
        $this->assertEquals("Next X-Men FiLm, X3, Delayed.", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testTrueCaptialsWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, delayed."|capitalize:true:true}');
        $this->assertEquals("Next X-Men Film, X3, Delayed.", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testQuotes()
    {
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \"delayed. umlauts\" foo."|capitalize}');
        $this->assertEquals("Next X-Men FiLm, x3, \"Delayed. Umlauts\" Foo.", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \'delayed. umlauts\' foo."|capitalize}');
        $this->assertEquals("Next X-Men FiLm, x3, 'Delayed. Umlauts' Foo.", $this->smarty->fetch($tpl));
    }

    public function testQuotesWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \"delayed. umlauts\" foo."|capitalize}');
        $this->assertEquals("Next X-Men FiLm, x3, \"Delayed. Umlauts\" Foo.", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \'delayed. umlauts\' foo."|capitalize}');
        $this->assertEquals("Next X-Men FiLm, x3, 'Delayed. Umlauts' Foo.", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testQuotesDigits()
    {
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \"delayed. umlauts\" foo."|capitalize:true}');
        $this->assertEquals("Next X-Men FiLm, X3, \"Delayed. Umlauts\" Foo.", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \'delayed. umlauts\' foo."|capitalize:true}');
        $this->assertEquals("Next X-Men FiLm, X3, 'Delayed. Umlauts' Foo.", $this->smarty->fetch($tpl));
    }

    public function testQuotesDigitsWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \"delayed. umlauts\" foo."|capitalize:true}');
        $this->assertEquals("Next X-Men FiLm, X3, \"Delayed. Umlauts\" Foo.", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \'delayed. umlauts\' foo."|capitalize:true}');
        $this->assertEquals("Next X-Men FiLm, X3, 'Delayed. Umlauts' Foo.", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testQuotesTrueCapitals()
    {
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \"delayed. umlauts\" foo."|capitalize:true:true}');
        $this->assertEquals("Next X-Men Film, X3, \"Delayed. Umlauts\" Foo.", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \'delayed. umlauts\' foo."|capitalize:true:true}');
        $this->assertEquals("Next X-Men Film, X3, 'Delayed. Umlauts' Foo.", $this->smarty->fetch($tpl));
    }

    public function testQuotesTrueCapitalsWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \"delayed. umlauts\" foo."|capitalize:true:true}');
        $this->assertEquals("Next X-Men Film, X3, \"Delayed. Umlauts\" Foo.", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('eval:{"next x-men fiLm, x3, \'delayed. umlauts\' foo."|capitalize:true:true}');
        $this->assertEquals("Next X-Men Film, X3, 'Delayed. Umlauts' Foo.", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }

    public function testQuotesBeginning()
    {
        $tpl = $this->smarty->createTemplate('eval:{"\"delayed. umlauts\" foo."|capitalize}');
        $this->assertEquals("\"Delayed. Umlauts\" Foo.", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('eval:{"\'delayed. umlauts\' foo."|capitalize}');
        $this->assertEquals("'Delayed. Umlauts' Foo.", $this->smarty->fetch($tpl));
    }

    public function testQuotesBeginningWithoutMbstring()
    {
        Smarty::$_MBSTRING = false;
        $tpl = $this->smarty->createTemplate('eval:{"\"delayed. umlauts\" foo."|capitalize}');
        $this->assertEquals("\"Delayed. Umlauts\" Foo.", $this->smarty->fetch($tpl));
        $tpl = $this->smarty->createTemplate('eval:{"\'delayed. umlauts\' foo."|capitalize}');
        $this->assertEquals("'Delayed. Umlauts' Foo.", $this->smarty->fetch($tpl));
        Smarty::$_MBSTRING = true;
    }
}
