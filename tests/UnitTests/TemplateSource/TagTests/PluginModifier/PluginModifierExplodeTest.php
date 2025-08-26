<?php

namespace UnitTests\TemplateSource\TagTests\PluginModifier;

/**
 * class for modifier tests
 *
 * 
 * 
 * 
 */
class PluginModifierExplodeTest extends \PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
        $this->smarty->registerPlugin('modifier', 'json_encode', 'json_encode');
    }

    /**
     * @deprecated
     * @dataProvider explodeDataProvider
     */
    public function testExplode($template = 'string:{","|explode:$subject|json_encode}', $subject = 'a,b,c,d', $expectedString = '["a","b","c","d"]')
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
                'template'      => 'string:{","|explode:$subject|json_encode}',
                'subject'       => 'a,b,c,d',
                'expectedString' => '["a","b","c","d"]',
            ],
            'withNoDelimiterFound'  => [
                'template'      => 'string:{","|explode:$subject|json_encode}',
                'subject'       => 'abcd',
                'expectedString' => '["abcd"]',
            ],
            'withNull' => [
                'template'      => 'string:{","|explode:$subject|json_encode}',
                'subject'       => null,
                'expectedString' => '[""]',
            ],
        ];
    }
}
