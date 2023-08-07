<?php

namespace UnitTests\TemplateSource\TagTests\PluginModifier;

/**
 * class for modifier tests
 *
 * 
 * 
 * 
 */
class PluginModifierReplaceTest extends \PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    /**
     * @return void
     * @throws \Smarty\Exception
     *
     * @dataProvider replaceDataProvider
     */
    public function testReplace($template, $subject, $expectedString)
    {
        $this->smarty->assign('subject', $subject);

        $tpl = $this->smarty->createTemplate($template);

        $this->assertEquals($expectedString, $this->smarty->fetch($tpl));
    }

    public function replaceDataProvider()
    {
        return [
            'default'  => [
                'template'      => 'string:{$subject|replace:",":"-"}',
                'subject'       => "a,b,c,d",
                'expectedString' => "a-b-c-d",
            ],
            'doNothing'  => [
                'template'      => 'string:{$subject|replace:"":""}',
                'subject'       => "a,b,c,d",
                'expectedString' => "a,b,c,d",
            ],
            'withNull' => [
                'template'      => 'string:{$subject|replace:"":""}',
                'subject'       => null,
                'expectedString' => "",
            ],
        ];
    }
}
