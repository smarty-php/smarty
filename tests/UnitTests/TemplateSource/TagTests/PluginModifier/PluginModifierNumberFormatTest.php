<?php

namespace UnitTests\TemplateSource\TagTests\PluginModifier;

/**
 * class for modifier tests
 *
 *
 *
 * 
 */
class PluginModifierNumberFormatTest extends \PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    /**
     * @return void
     * @throws \Smarty\Exception
     *
     * @dataProvider numberFormatDataProvider
     */
    public function testNumberFormat($template = 'string:{$subject|number_format}', $subject = 12345, $expectedString = "12,345")
    {
        $this->smarty->assign('subject', $subject);

        $tpl = $this->smarty->createTemplate($template);

        $this->assertEquals($expectedString, $this->smarty->fetch($tpl));
    }

    public function numberFormatDataProvider()
    {
        return [
            'default'  => [
                'template'      => 'string:{$subject|number_format}',
                'subject'       => 12345,
                'expectedString' => "12,345",
            ],
            'withDecimalDefault'  => [
                'template'      => 'string:{$subject|number_format}',
                'subject'       => 12345.6789,
                'expectedString' => "12,346",
            ],
            'withDecimalAndExtras'  => [
                'template'      => 'string:{$subject|number_format:2:"-":"~"}',
                'subject'       => 12345.6789,
                'expectedString' => "12~345-68",
            ],
            'withNull' => [
                'template'      => 'string:{$subject|number_format}',
                'subject'       => null,
                'expectedString' => 0,
            ],
        ];
    }
}
