<?php

     /**
     * test registered pre filter closure
     * @requires PHP 5.3
     */

        $this->smarty->registerFilter(\Smarty\Smarty::FILTER_PRE, function ($input) {
            return '{$foo}' . $input;
        });
        $tpl = $this->smarty->createTemplate('eval:{" hello world"}');
        $tpl->assign('foo', 'buh');
        $this->assertEquals("buh hello world", $this->smarty->fetch($tpl));

