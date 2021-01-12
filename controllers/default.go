package controllers

import (
	beego "github.com/beego/beego/v2/server/web"
)

type MainController struct {
	beego.Controller
}

func (this *MainController) Get() {
	this.Data["Title"] = "beego.me"
	this.TplName = "index.tpl"
}
