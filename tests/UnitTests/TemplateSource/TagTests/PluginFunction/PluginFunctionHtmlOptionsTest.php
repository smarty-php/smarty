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
class PluginFunctionHtmlOptionsTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testAssociativeArray()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect}');
        $tpl->assign('mySelect', 9904);
        $tpl->assign('myOptions', array(
            1800 => 'Joe Schmoe',
            9904 => 'Jack Smith',
            2003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testSeparateArrays()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="56">Joe Schmoe</option>'
            . $n . '<option value="92" selected="selected">Jane Johnson</option>'
            . $n . '<option value="13">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" values=$cust_ids output=$cust_names selected=$customer_id}');
        $tpl->assign('customer_id', 92);
        $tpl->assign('cust_ids', array(56, 92, 13));
        $tpl->assign('cust_names', array(
            'Joe Schmoe',
            'Jane Johnson',
            'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testIterator()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect}');
        $tpl->assign('mySelect', 9904);
        $tpl->assign('myOptions', new ArrayIterator(array(
                                                        1800 => 'Joe Schmoe',
                                                        9904 => 'Jack Smith',
                                                        2003 => 'Charlie Brown',
                                                    )));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testOptgroup()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<optgroup label="Sport">'
            . $n . '<option value="6">Golf</option>'
            . $n . '<option value="9">Cricket</option>'
            . $n . '<option value="7" selected="selected">Swim</option>'
            . $n . '</optgroup>'
            . $n . '<optgroup label="Rest">'
            . $n . '<option value="3">Sauna</option>'
            . $n . '<option value="1">Massage</option>'
            . $n . '</optgroup>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name=foo options=$lookups selected=$fav}');
        $tpl->assign('fav', 7);
        $tpl->assign('lookups', array(
            'Sport' => array(
                6 => 'Golf',
                9 => 'Cricket',
                7 => 'Swim'
            ),
            'Rest'  => array(
                3 => 'Sauna',
                1 => 'Massage'
            ),
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testNullString()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="null" selected="selected">null</option>'
            . $n . '<option value="0">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '<optgroup label="optgroup">'
            . $n . '<option value="null" selected="selected">null</option>'
            . $n . '<option value="0">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '</optgroup>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name=foo options=$array selected=$selected}');
        $tpl->assign('selected', "null");
        $tpl->assign('array', array(
            'null'     => 'null',
            0          => 'zero',
            1          => 'one',
            2          => 'two',
            'optgroup' => array(
                'null' => 'null',
                0      => 'zero',
                1      => 'one',
                2      => 'two',
            ),
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testNullValue()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="" selected="selected">empty string</option>'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '<optgroup label="optgroup">'
            . $n . '<option value="" selected="selected">empty string</option>'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '</optgroup>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name=foo options=$array selected=$selected}');
        $tpl->assign('selected', null);
        $tpl->assign('array', array(
            ''         => 'empty string',
            'null'     => 'null',
            0          => 'zero',
            1          => 'one',
            2          => 'two',
            'optgroup' => array(
                ''     => 'empty string',
                'null' => 'null',
                0      => 'zero',
                1      => 'one',
                2      => 'two',
            ),
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testZeroValue()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0" selected="selected">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '<optgroup label="optgroup">'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0" selected="selected">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '</optgroup>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name=foo options=$array selected=$selected}');
        $tpl->assign('selected', 0);
        $tpl->assign('array', array(
            'null'     => 'null',
            0          => 'zero',
            1          => 'one',
            2          => 'two',
            'optgroup' => array(
                'null' => 'null',
                0      => 'zero',
                1      => 'one',
                2      => 'two',
            ),
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testZeroStringValue()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0" selected="selected">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '<optgroup label="optgroup">'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0" selected="selected">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '</optgroup>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name=foo options=$array selected=$selected}');
        $tpl->assign('selected', "0");
        $tpl->assign('array', array(
            'null'     => "null",
            0          => 'zero',
            1          => 'one',
            2          => 'two',
            'optgroup' => array(
                'null' => 'null',
                0      => 'zero',
                1      => 'one',
                2      => 'two',
            ),
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testEmptyStringValue()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '<optgroup label="optgroup">'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '</optgroup>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name=foo options=$array selected=$selected}');
        $tpl->assign('selected', "");
        $tpl->assign('array', array(
            'null'     => 'null',
            '0'        => 'zero',
            '1'        => 'one',
            '2'        => 'two',
            'optgroup' => array(
                'null' => 'null',
                0      => 'zero',
                1      => 'one',
                2      => 'two',
            ),
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testEmptyStringValues()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="" selected="selected">empty string</option>'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '<optgroup label="optgroup">'
            . $n . '<option value="" selected="selected">empty string</option>'
            . $n . '<option value="null">null</option>'
            . $n . '<option value="0">zero</option>'
            . $n . '<option value="1">one</option>'
            . $n . '<option value="2">two</option>'
            . $n . '</optgroup>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name=foo options=$array selected=$selected}');
        $tpl->assign('selected', "");
        $tpl->assign('array', array(
            ''         => 'empty string',
            'null'     => 'null',
            '0'        => 'zero',
            '1'        => 'one',
            '2'        => 'two',
            'optgroup' => array(
                ''     => 'empty string',
                'null' => 'null',
                0      => 'zero',
                1      => 'one',
                2      => 'two',
            ),
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testObject()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect}');
        $tpl->assign('mySelect', new _object_toString(9904));
        $tpl->assign('myOptions', array(
            1800 => 'Joe Schmoe',
            9904 => 'Jack Smith',
            2003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testObjectList()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect}');
        $tpl->assign('mySelect', new _object_toString(9904));
        $tpl->assign('myOptions', array(
            1800 => new _object_toString('Joe Schmoe'),
            9904 => new _object_toString('Jack Smith'),
            2003 => new _object_toString('Charlie Brown'),
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
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect}');
        $tpl->assign('mySelect', new _object_noString(9904));
        $tpl->assign('myOptions', array(
            1800 => 'Joe Schmoe',
            9904 => 'Jack Smith',
            2003 => 'Charlie Brown',
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
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect}');
        $tpl->assign('mySelect', new _object_toString(9904));
        $tpl->assign('myOptions', array(
            1800 => new _object_toString('Joe Schmoe'),
            9904 => new _object_noString('Jack Smith'),
            2003 => new _object_toString('Charlie Brown'),
        ));

        $tpl->fetch();
        $this->assertEquals(1, count($this->_errors));
        $this->assertStringEndsWith("without __toString() method", $this->_errors[0]);

        restore_error_handler();
    }

    public function testDisabled()
    {
        $n = "\n";
        $expected = '<select name="foo" disabled="1">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect disabled=1}');
        $tpl->assign('mySelect', new _object_toString(9904));
        $tpl->assign('myOptions', array(
            1800 => 'Joe Schmoe',
            9904 => 'Jack Smith',
            2003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }

    public function testDisabledStrict()
    {
        $n = "\n";
        $expected = '<select name="foo">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect disabled=1 strict=true}');
        $tpl->assign('mySelect', new _object_toString(9904));
        $tpl->assign('myOptions', array(
            1800 => 'Joe Schmoe',
            9904 => 'Jack Smith',
            2003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());

        $n = "\n";
        $expected = '<select name="foo" disabled="disabled">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect disabled=true strict=true}');
        $tpl->assign('mySelect', new _object_toString(9904));
        $tpl->assign('myOptions', array(
            1800 => 'Joe Schmoe',
            9904 => 'Jack Smith',
            2003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());

        $n = "\n";
        $expected = '<select name="foo" disabled="disabled">'
            . $n . '<option value="1800">Joe Schmoe</option>'
            . $n . '<option value="9904" selected="selected">Jack Smith</option>'
            . $n . '<option value="2003">Charlie Brown</option>'
            . $n . '</select>' . $n;

        $tpl = $this->smarty->createTemplate('eval:{html_options name="foo" options=$myOptions selected=$mySelect disabled="disabled" strict=true}');
        $tpl->assign('mySelect', new _object_toString(9904));
        $tpl->assign('myOptions', array(
            1800 => 'Joe Schmoe',
            9904 => 'Jack Smith',
            2003 => 'Charlie Brown',
        ));

        $this->assertEquals($expected, $tpl->fetch());
    }
}
