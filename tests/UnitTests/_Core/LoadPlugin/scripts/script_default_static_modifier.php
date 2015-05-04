<?php
if (!class_exists('DefModifier')) {
    Class DefModifier
    {
        static function default_static_modifier($input)
        {
            return 'staticmodifier ' . $input;
        }
    }
}
