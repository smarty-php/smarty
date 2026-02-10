{* Test template for NEW function object chain feature *}
{* Test: function()->method()->method()->method() *}
{collect($data)->filter()->values()->toJson()}
{collect($data)->filter()->count()}