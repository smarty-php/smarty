<?php
/**
 * Smarty PHPunit tests of modifier
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

require_once(dirname(__FILE__) . '/helpers/_object_tostring.php');

/**
 * class for modifier tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class PluginFunctionHtmlCheckboxesTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testAssociativeArray()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />"}');
        $tpl->assign('customer_id', 1001);
        $tpl->assign('cust_radios', array(
            1000 => 'Joe Schmoe',
            1001 => 'Jack Smith',
            1002 => 'Jane Johnson',
            1003 => 'Charlie Brown',
        ));
        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testSeparateArrays()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" values=$cust_ids output=$cust_names selected=$customer_id separator="<br />"}');
        $tpl->assign('customer_id', 1001);
        $tpl->assign('cust_ids', array(1000, 1001, 1002, 1003));
        $tpl->assign('cust_names', array(
            'Joe Schmoe',
            'Jack Smith',
            'Jane Johnson',
            'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testIterator()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" values=$cust_ids output=$cust_names selected=$customer_id separator="<br />"}');
        $tpl->assign('customer_id', 1001);
        $tpl->assign('cust_ids', array(1000, 1001, 1002, 1003));
        $tpl->assign('cust_names', new ArrayIterator(array(
                                                         'Joe Schmoe',
                                                         'Jack Smith',
                                                         'Jane Johnson',
                                                         'Charlie Brown',
                                                     )));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testNoLabels()
    {
        $n = "\n";
        $expected = '<input type="checkbox" name="id[]" value="1000" />Joe Schmoe<br />'
            . $n . '<input type="checkbox" name="id[]" value="1001" checked="checked" />Jack Smith<br />'
            . $n . '<input type="checkbox" name="id[]" value="1002" />Jane Johnson<br />'
            . $n . '<input type="checkbox" name="id[]" value="1003" />Charlie Brown<br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios labels=false selected=$customer_id separator="<br />"}');
        $tpl->assign('customer_id', 1001);
        $tpl->assign('cust_radios', array(
            1000 => 'Joe Schmoe',
            1001 => 'Jack Smith',
            1002 => 'Jane Johnson',
            1003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testWithId()
    {
        $n = "\n";
        $expected = '<label for="id_1000"><input type="checkbox" name="id[]" value="1000" id="id_1000" />Joe Schmoe</label><br />'
            . $n . '<label for="id_1001"><input type="checkbox" name="id[]" value="1001" id="id_1001" checked="checked" />Jack Smith</label><br />'
            . $n . '<label for="id_1002"><input type="checkbox" name="id[]" value="1002" id="id_1002" />Jane Johnson</label><br />'
            . $n . '<label for="id_work_s_ä"><input type="checkbox" name="id[]" value="work s ä" id="id_work_s_ä" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id label_ids=true separator="<br />"}');
        $tpl->assign('customer_id', 1001);
        $tpl->assign('cust_radios', array(
            1000       => 'Joe Schmoe',
            1001       => 'Jack Smith',
            1002       => 'Jane Johnson',
            'work s ä' => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testNullString()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="null" checked="checked" />null</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="" />empty string</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="0" />zero</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1" />one</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="2" />two</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$options selected=$selected separator="<br />"}');
        $tpl->assign('selected', "null");
        $tpl->assign('options', array(
            "null" => 'null',
            ''     => 'empty string',
            0      => 'zero',
            1      => 'one',
            2      => 'two',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testNullValue()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="null" />null</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="" checked="checked" />empty string</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="0" />zero</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1" />one</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="2" />two</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$options selected=$selected separator="<br />"}');
        $tpl->assign('selected', null);
        $tpl->assign('options', array(
            "null" => 'null',
            ''     => 'empty string',
            0      => 'zero',
            1      => 'one',
            2      => 'two',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testZeroValue()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="null" />null</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="" />empty string</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="0" checked="checked" />zero</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1" />one</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="2" />two</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$options selected=$selected separator="<br />"}');
        $tpl->assign('selected', 0);
        $tpl->assign('options', array(
            "null" => 'null',
            ''     => 'empty string',
            0      => 'zero',
            1      => 'one',
            2      => 'two',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testZeroStringValue()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="null" />null</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="" />empty string</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="0" checked="checked" />zero</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1" />one</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="2" />two</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$options selected=$selected separator="<br />"}');
        $tpl->assign('selected', "0");
        $tpl->assign('options', array(
            "null" => 'null',
            ''     => 'empty string',
            0      => 'zero',
            1      => 'one',
            2      => 'two',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testEmptyStringValue()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="null" />null</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="" checked="checked" />empty string</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="0" />zero</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1" />one</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="2" />two</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$options selected=$selected separator="<br />"}');
        $tpl->assign('selected', "");
        $tpl->assign('options', array(
            "null" => 'null',
            ''     => 'empty string',
            0      => 'zero',
            1      => 'one',
            2      => 'two',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testObject()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />"}');
        $tpl->assign('customer_id', new _object_toString(1001));
        $tpl->assign('cust_radios', array(
            1000 => 'Joe Schmoe',
            1001 => 'Jack Smith',
            1002 => 'Jane Johnson',
            1003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testObjectList()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />"}');
        $tpl->assign('customer_id', 1001);
        $tpl->assign('cust_radios', array(
            1000 => new _object_toString('Joe Schmoe'),
            1001 => new _object_toString('Jack Smith'),
            1002 => new _object_toString('Jane Johnson'),
            1003 => new _object_toString('Charlie Brown'),
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    protected $_errors = array();

    public function error_handler($errno, $errstr, $errfile, $errline, $errcontext = array())
    {
        $this->_errors[] = $errstr;
    }

    public function testObjectNoString()
    {
        $this->_errors = array();
        set_error_handler(array($this, 'error_handler'));

        $this->smarty->muteUndefinedOrNullWarnings();
        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />"}');
        $tpl->assign('customer_id', new _object_noString(1001));
        $tpl->assign('cust_radios', array(
            1000 => 'Joe Schmoe',
            1001 => 'Jack Smith',
            1002 => 'Jane Johnson',
            1003 => 'Charlie Brown',
        ));

        $tpl->fetch();
        $this->assertEquals(1, count($this->_errors));
        $this->assertStringEndsWith("without __toString() method", $this->_errors[0]);

        restore_error_handler();
    }

    public function testObjectListNoString()
    {
        $this->_errors = array();
        set_error_handler(array($this, 'error_handler'));

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />"}');
        $tpl->assign('customer_id', 1001);
        $tpl->assign('cust_radios', array(
            1000 => new _object_toString('Joe Schmoe'),
            1001 => new _object_noString('Jack Smith'),
            1002 => new _object_toString('Jane Johnson'),
            1003 => new _object_toString('Charlie Brown'),
        ));

        $tpl->fetch();
        $this->assertEquals(1, count($this->_errors));
        $this->assertStringEndsWith("without __toString() method", $this->_errors[0]);

        restore_error_handler();
    }

    public function testDisabled()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" disabled="1" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" disabled="1" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" disabled="1" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" disabled="1" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />" disabled="1"}');
        $tpl->assign('customer_id', new _object_toString(1001));
        $tpl->assign('cust_radios', array(
            1000 => 'Joe Schmoe',
            1001 => 'Jack Smith',
            1002 => 'Jane Johnson',
            1003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testDisabledStrict1()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" disabled="disabled" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" disabled="disabled" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" disabled="disabled" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" disabled="disabled" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />" disabled=true strict=true}');
        $tpl->assign('customer_id', new _object_toString(1001));
        $tpl->assign('cust_radios', array(
            1000 => 'Joe Schmoe',
            1001 => 'Jack Smith',
            1002 => 'Jane Johnson',
            1003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }
    public function testDisabledStrict2()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />" disabled=1 strict=true}');
        $tpl->assign('customer_id', new _object_toString(1001));
        $tpl->assign('cust_radios', array(
            1000 => 'Joe Schmoe',
            1001 => 'Jack Smith',
            1002 => 'Jane Johnson',
            1003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());

     }
    public function testDisabledStrict3()
    {
        $n = "\n";
        $expected = '<label><input type="checkbox" name="id[]" value="1000" disabled="disabled" />Joe Schmoe</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1001" checked="checked" disabled="disabled" />Jack Smith</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1002" disabled="disabled" />Jane Johnson</label><br />'
            . $n . '<label><input type="checkbox" name="id[]" value="1003" disabled="disabled" />Charlie Brown</label><br />';

        $tpl = $this->smarty->createTemplate('eval:{html_checkboxes name="id" options=$cust_radios selected=$customer_id separator="<br />" disabled="disabled" strict=true}');
        $tpl->assign('customer_id', new _object_toString(1001));
        $tpl->assign('cust_radios', array(
            1000 => 'Joe Schmoe',
            1001 => 'Jack Smith',
            1002 => 'Jane Johnson',
            1003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }
}
