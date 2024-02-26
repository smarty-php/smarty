<?php

namespace UnitTests\TemplateSource\TagTests\PluginModifier;

/**
 * class for modifier tests
 *
 * 
 * 
 * 
 */
class PluginModifierSplitTest extends \PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->registerPlugin('modifier', 'json_encode', 'json_encode');
    }

    /**
     * @dataProvider explodeDataProvider
     */
    public function testSplit($template, $subject, $expectedString)
    {
        $this->smarty->assign('subject', $subject);

        $tpl = $this->smarty->createTemplate($template);
        $res = $this->smarty->fetch($tpl);

        $this->assertEquals($expectedString, $res);
    }

    public function explodeDataProvider()
    {
        return [
            'default'  => [
                'template'      => 'string:{$subject|split:","|json_encode}',
                'subject'       => 'a,b,c,d',
                'expectedString' => '["a","b","c","d"]',
            ],
            'withNoDelimiterFound'  => [
                'template'      => 'string:{$subject|split:","|json_encode}',
                'subject'       => 'abcd',
                'expectedString' => '["abcd"]',
            ],
            'withNull' => [
                'template'      => 'string:{$subject|split:","|json_encode}',
                'subject'       => null,
                'expectedString' => '[""]',
            ],
        ];
    }
}
