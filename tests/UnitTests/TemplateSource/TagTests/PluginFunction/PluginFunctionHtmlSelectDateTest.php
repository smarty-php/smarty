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
class PluginFunctionHtmlSelectDateTest extends PHPUnit_Smarty
{
    public $now = null;
    protected $years = array();
    protected $months = array(
        'none'            => '<option value="01">January</option>
<option value="02">February</option>
<option value="03">March</option>
<option value="04">April</option>
<option value="05">May</option>
<option value="06">June</option>
<option value="07">July</option>
<option value="08">August</option>
<option value="09">September</option>
<option value="10">October</option>
<option value="11">November</option>
<option value="12">December</option>',
        'default'         => '<option value="01">January</option>
<option value="02" selected="selected">February</option>
<option value="03">March</option>
<option value="04">April</option>
<option value="05">May</option>
<option value="06">June</option>
<option value="07">July</option>
<option value="08">August</option>
<option value="09">September</option>
<option value="10">October</option>
<option value="11">November</option>
<option value="12">December</option>',
        'format_%b'       => '<option value="01">Jan</option>
<option value="02" selected="selected">Feb</option>
<option value="03">Mar</option>
<option value="04">Apr</option>
<option value="05">May</option>
<option value="06">Jun</option>
<option value="07">Jul</option>
<option value="08">Aug</option>
<option value="09">Sep</option>
<option value="10">Oct</option>
<option value="11">Nov</option>
<option value="12">Dec</option>',
        'format_value_%b' => '<option value="Jan">January</option>
<option value="Feb" selected="selected">February</option>
<option value="Mar">March</option>
<option value="Apr">April</option>
<option value="May">May</option>
<option value="Jun">June</option>
<option value="Jul">July</option>
<option value="Aug">August</option>
<option value="Sep">September</option>
<option value="Oct">October</option>
<option value="Nov">November</option>
<option value="Dec">December</option>',
        'names'           => '<option value="01">alpha</option>
<option value="02" selected="selected">bravo</option>
<option value="03">charlie</option>
<option value="04">delta</option>
<option value="05">echo</option>
<option value="06">foxtrot</option>
<option value="07">golf</option>
<option value="08">hotel</option>
<option value="09">india</option>
<option value="10">juliet</option>
<option value="11">kilo</option>
<option value="12">lima</option>',
    );

    protected $days = array(
        'none'              => '<option value="1">01</option>
<option value="2">02</option>
<option value="3">03</option>
<option value="4">04</option>
<option value="5">05</option>
<option value="6">06</option>
<option value="7">07</option>
<option value="8">08</option>
<option value="9">09</option>
<option value="10">10</option>
<option value="11">11</option>
<option value="12">12</option>
<option value="13">13</option>
<option value="14">14</option>
<option value="15">15</option>
<option value="16">16</option>
<option value="17">17</option>
<option value="18">18</option>
<option value="19">19</option>
<option value="20">20</option>
<option value="21">21</option>
<option value="22">22</option>
<option value="23">23</option>
<option value="24">24</option>
<option value="25">25</option>
<option value="26">26</option>
<option value="27">27</option>
<option value="28">28</option>
<option value="29">29</option>
<option value="30">30</option>
<option value="31">31</option>',
        'default'           => '<option value="1">01</option>
<option value="2">02</option>
<option value="3">03</option>
<option value="4">04</option>
<option value="5">05</option>
<option value="6">06</option>
<option value="7">07</option>
<option value="8">08</option>
<option value="9">09</option>
<option value="10">10</option>
<option value="11">11</option>
<option value="12">12</option>
<option value="13">13</option>
<option value="14">14</option>
<option value="15">15</option>
<option value="16">16</option>
<option value="17">17</option>
<option value="18">18</option>
<option value="19">19</option>
<option value="20" selected="selected">20</option>
<option value="21">21</option>
<option value="22">22</option>
<option value="23">23</option>
<option value="24">24</option>
<option value="25">25</option>
<option value="26">26</option>
<option value="27">27</option>
<option value="28">28</option>
<option value="29">29</option>
<option value="30">30</option>
<option value="31">31</option>',
        'format_%03d'       => '<option value="1">001</option>
<option value="2">002</option>
<option value="3">003</option>
<option value="4">004</option>
<option value="5">005</option>
<option value="6">006</option>
<option value="7">007</option>
<option value="8">008</option>
<option value="9">009</option>
<option value="10">010</option>
<option value="11">011</option>
<option value="12">012</option>
<option value="13">013</option>
<option value="14">014</option>
<option value="15">015</option>
<option value="16">016</option>
<option value="17">017</option>
<option value="18">018</option>
<option value="19">019</option>
<option value="20" selected="selected">020</option>
<option value="21">021</option>
<option value="22">022</option>
<option value="23">023</option>
<option value="24">024</option>
<option value="25">025</option>
<option value="26">026</option>
<option value="27">027</option>
<option value="28">028</option>
<option value="29">029</option>
<option value="30">030</option>
<option value="31">031</option>',
        'format_value_%03d' => '<option value="001">01</option>
<option value="002">02</option>
<option value="003">03</option>
<option value="004">04</option>
<option value="005">05</option>
<option value="006">06</option>
<option value="007">07</option>
<option value="008">08</option>
<option value="009">09</option>
<option value="010">10</option>
<option value="011">11</option>
<option value="012">12</option>
<option value="013">13</option>
<option value="014">14</option>
<option value="015">15</option>
<option value="016">16</option>
<option value="017">17</option>
<option value="018">18</option>
<option value="019">19</option>
<option value="020" selected="selected">20</option>
<option value="021">21</option>
<option value="022">22</option>
<option value="023">23</option>
<option value="024">24</option>
<option value="025">25</option>
<option value="026">26</option>
<option value="027">27</option>
<option value="028">28</option>
<option value="029">29</option>
<option value="030">30</option>
<option value="031">31</option>',
    );

    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
	    $this->smarty->setErrorReporting(E_ALL & ~E_DEPRECATED);

        $year = date('Y');
        $this->now = mktime(15, 0, 0, 2, 20, $year);
        $o = '';
        for ($i = 2005; $i < $year; $i ++) {
            $o .= "<option value=\"{$i}\">{$i}</option>\n";
        }
        $o .= "<option value=\"{$year}\" selected=\"selected\">{$year}</option>";
        $this->years['start_2005'] = $o;
        $this->years['end_2005'] = $o;

        $o = "<option value=\"{$year}\" selected=\"selected\">{$year}</option>";
        for ($i = $year + 1; $i < $year + 6; $i ++) {
            $o .= "\n<option value=\"{$i}\">{$i}</option>";
        }
        $this->years['start_+5'] = $o;
        $this->years['end_+5'] = $o;

        $o = '';
        for ($i = $year - 5; $i < $year; $i ++) {
            $o .= "<option value=\"{$i}\">{$i}</option>\n";
        }
        $o .= "<option value=\"{$year}\" selected=\"selected\">{$year}</option>";
        $this->years['start_-5'] = $o;
        $this->years['end_-5'] = $o;

        $this->years['default'] = "<option value=\"{$year}\" selected=\"selected\">{$year}</option>";
        $this->years['none'] = "<option value=\"{$year}\">{$year}</option>";
    }

    protected function reverse($string)
    {
        $t = explode("\n", $string);
        $t = array_reverse($t);

        return join("\n", $t);
    }

    public function testDefault()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . '}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testPrefix()
    {
        $n = "\n";
        $result = '<select name="foobar_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="foobar_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="foobar_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' prefix="foobar_"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testFieldArray()
    {
        $n = "\n";
        $result = '<select name="namorized[Date_Month]">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="namorized[Date_Day]">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="namorized[Date_Year]">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' field_array="namorized"}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="namorized[foobar_Month]">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="namorized[foobar_Day]">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="namorized[foobar_Year]">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' field_array="namorized" prefix="foobar_"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testExtra()
    {
        $n = "\n";
        $result = '<select name="Date_Month" data-foo="xy">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day" data-foo="xy">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year" data-foo="xy">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' all_extra="data-foo=\"xy\""}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month" data-foo="month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day" data-foo="day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year" data-foo="year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' day_extra="data-foo=\"day\"" month_extra="data-foo=\"month\"" year_extra="data-foo=\"year\""}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month" data_foo="foo">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day" data_foo="foo">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year" data_foo="foo">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' data_foo="foo"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testFieldOrder()
    {
        $n = "\n";
        $result = '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' field_order="DMY"}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>'
            . $n . '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' field_order="YMD"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testFieldSeparator()
    {
        $n = "\n";
        $result = '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . ' - <select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . ' - <select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' field_order="DMY" field_separator=" - "}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>'
            . ' / <select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . ' / <select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' field_order="YMD" field_separator=" / "}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testEmpty()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . '<option value=""></option>' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . '<option value=""></option>' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . '<option value=""></option>' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' all_empty=""}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month">' . $n . '<option value="">all</option>' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . '<option value="">all</option>' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . '<option value="">all</option>' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' all_empty="all"}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . '<option value=""></option>' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' year_empty=""}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month">' . $n . '<option value="">month</option>' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . '<option value="">day</option>' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . '<option value="">year</option>' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' year_empty="year" month_empty="month" day_empty="day"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testEmptyUnset()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . '<option value=""></option>' . $n . $this->months['none'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . '<option value=""></option>' . $n . $this->days['none'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . '<option value=""></option>' . $n . $this->years['none'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=null all_empty=""}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month">' . $n . '<option value="">all</option>' . $n . $this->months['none'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . '<option value="">all</option>' . $n . $this->days['none'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . '<option value="">all</option>' . $n . $this->years['none'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=null all_empty="all"}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month">' . $n . $this->months['none'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['none'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . '<option value=""></option>' . $n . $this->years['none'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=null year_empty=""}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month">' . $n . '<option value="">month</option>' . $n . $this->months['none'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . '<option value="">day</option>' . $n . $this->days['none'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . '<option value="">year</option>' . $n . $this->years['none'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=null year_empty="year" month_empty="month" day_empty="day"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testId()
    {
        $n = "\n";
        $result = '<select name="Date_Month" id="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day" id="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year" id="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' all_id=""}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month" id="all-Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day" id="all-Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year" id="all-Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' all_id="all-"}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month" id="month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day" id="day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year" id="year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' year_id="year" month_id="month" day_id="day"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testStartYearAbsolute()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['start_2005'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' start_year=2005}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testStartYearRelative()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['start_+5'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' start_year="+5"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testStartYearRelativeNegative()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['start_-5'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' start_year="-5"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testEndYearAbsolute()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['end_2005'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' end_year=2005}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testEndYearRelative()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['end_+5'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' end_year="+5"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testEndYearRelativeNegative()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['end_-5'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' end_year="-5"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testDisplayDaysMonthYear()
    {
        $n = "\n";
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' display_days=false}');
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $this->assertEquals($result, $tpl->fetch());

        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' display_months=false}');
        $result = '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $this->assertEquals($result, $tpl->fetch());

        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' display_years=false}');
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>';
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testYearsReversed()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->reverse($this->years['start_2005']) . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' start_year=2005 reverse_years=true}');
        $this->assertEquals($result, $tpl->fetch());

        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->reverse($this->years['start_+5']) . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' start_year="+5" reverse_years=true}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testYearText()
    {
        $year = date('Y');
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . "<input type=\"text\" name=\"Date_Year\" value=\"{$year}\" size=\"4\" maxlength=\"4\" />";
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' year_as_text=true}');
        $this->assertEquals(str_replace("\r", '', $result), $this->smarty->fetch($tpl));

        $result = '<select name="foo_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="foo_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . "<input type=\"text\" name=\"foo_Year\" value=\"{$year}\" size=\"4\" maxlength=\"4\" />";
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' year_as_text=true prefix="foo_"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testMonthFormat()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['format_%b'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' month_format="%b"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testMonthFormatValue()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['format_value_%b'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' month_value_format="%b"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testMonthNames()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['names'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{$names = [1 => "alpha","bravo","charlie","delta","echo","foxtrot","golf","hotel","india","juliet","kilo","lima"]}{html_select_date time=' . $this->now . ' month_names=$names}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testDayFormat()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['format_%03d'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' day_format="%03d"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testDayFormatValue()
    {
        $n = "\n";
        $result = '<select name="Date_Month">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="Date_Day">' . $n . $this->days['format_value_%03d'] . $n . '</select>'
            . $n . '<select name="Date_Year">' . $n . $this->years['default'] . $n . '</select>';
        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=' . $this->now . ' day_value_format="%03d"}');
        $this->assertEquals($result, $tpl->fetch());
    }

    public function testTimeArray()
    {
        $n = "\n";
        $result = '<select name="namorized[foobar_Month]">' . $n . $this->months['default'] . $n . '</select>'
            . $n . '<select name="namorized[foobar_Day]">' . $n . $this->days['default'] . $n . '</select>'
            . $n . '<select name="namorized[foobar_Year]">' . $n . $this->years['default'] . $n . '</select>';

        $date_array = array(
            'namorized' => array(
                'foobar_Month' => '02',
                'foobar_Day'   => '20',
                'foobar_Year'  => date('Y'),
            ),
        );

        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=$date_array.namorized field_array="namorized" prefix="foobar_"}');
        $tpl->assign('date_array', $date_array);
        $this->assertEquals($result, $tpl->fetch());

        $tpl = $this->smarty->createTemplate('eval:{html_select_date time=$date_array field_array="namorized" prefix="foobar_"}');
        $tpl->assign('date_array', $date_array);
        $this->assertEquals($result, $tpl->fetch());
    }
}
