<?php
/**
 * {html_options}, {html_select_date} and {html_select_time} must HTML-escape every parameter value
 * they write into the page, so a value that comes from application data cannot add attributes or
 * elements.
 */
class HtmlFormFunctionEscapingTest extends PHPUnit_Smarty
{
    const PAYLOAD = 'x"><b>';
    const ESCAPED = 'x&quot;&gt;&lt;b&gt;';

    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    /**
     * @dataProvider dataParameters
     */
    public function testParameterIsEscaped($template)
    {
        $this->smarty->assign('p', self::PAYLOAD);
        $this->smarty->assign('fmt', self::PAYLOAD . '%d');
        $output = $this->smarty->fetch('string:' . $template);
        $this->assertStringNotContainsString('<b>', $output);
        $this->assertStringContainsString(self::ESCAPED, $output);
    }

    public function dataParameters()
    {
        $date = '{html_select_date time="2020-05-06" ';
        $time = '{html_select_time time="10:20:30" ';
        return [
            'html_options name' => ['{html_options name=$p options=[1=>"a"]}'],
            'html_options id' => ['{html_options name="n" id=$p options=[1=>"a"]}'],
            'html_options class' => ['{html_options name="n" class=$p options=[1=>"a"]}'],
            'html_options id without name' => ['{html_options id=$p options=[1=>"a"]}'],
            'html_options class without name' => ['{html_options class=$p options=[1=>"a"]}'],
            'html_select_date prefix' => [$date . 'prefix=$p}'],
            'html_select_date field_array' => [$date . 'field_array=$p}'],
            'html_select_date all_empty' => [$date . 'all_empty=$p}'],
            'html_select_date year_empty' => [$date . 'year_empty=$p}'],
            'html_select_date month_empty' => [$date . 'month_empty=$p}'],
            'html_select_date day_empty' => [$date . 'day_empty=$p}'],
            'html_select_date day_format' => [$date . 'day_format=$fmt}'],
            'html_select_date day_value_format' => [$date . 'day_value_format=$fmt}'],
            'html_select_date year_as_text prefix' => [$date . 'year_as_text=true prefix=$p}'],
            'html_select_time prefix' => [$time . 'prefix=$p}'],
            'html_select_time field_array' => [$time . 'field_array=$p}'],
            'html_select_time all_empty' => [$time . 'all_empty=$p}'],
            'html_select_time hour_empty' => [$time . 'hour_empty=$p}'],
            'html_select_time minute_empty' => [$time . 'minute_empty=$p}'],
            'html_select_time second_empty' => [$time . 'second_empty=$p}'],
            'html_select_time meridian_empty' => [$time . 'use_24_hours=false meridian_empty=$p}'],
            'html_select_time hour_format' => [$time . 'hour_format=$fmt}'],
            'html_select_time hour_value_format' => [$time . 'hour_value_format=$fmt}'],
            'html_select_time minute_format' => [$time . 'minute_format=$fmt}'],
            'html_select_time minute_value_format' => [$time . 'minute_value_format=$fmt}'],
            'html_select_time second_format' => [$time . 'second_format=$fmt}'],
            'html_select_time second_value_format' => [$time . 'second_value_format=$fmt}'],
        ];
    }

    public function testYearAsTextValueFromTimeArrayIsEscaped()
    {
        $this->smarty->assign('t', ['Date_Year' => self::PAYLOAD]);
        $output = $this->smarty->fetch(
            'string:{html_select_date time=$t year_as_text=true display_days=false display_months=false}'
        );
        $this->assertStringNotContainsString('<b>', $output);
        $this->assertStringContainsString('value="' . self::ESCAPED . '"', $output);
    }

    public function testExistingEntitiesAndArrayNamesAreKept()
    {
        $output = $this->smarty->fetch(
            'string:{html_select_date time="2020-05-06" field_array="f" prefix="p_" all_empty="&nbsp;"}'
            . '{html_options name="n[]" options=[1=>"a"]}'
        );
        $this->assertStringContainsString('<select name="f[p_Year]"', $output);
        $this->assertStringContainsString('<option value="">&nbsp;</option>', $output);
        $this->assertStringContainsString('<select name="n[]"', $output);
    }
}
