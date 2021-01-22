package routers

import (
	"duretoryGo/controllers"

	beego "github.com/beego/beego/v2/server/web"
)

func init() {
	beego.Router("/", &controllers.MainController{})
	beego.Router("/bookmarks", &controllers.MainController{})
	beego.Router("/login", &controllers.MainController{})
	beego.Router("/forget", &controllers.MainController{})
	beego.Router("/code", &controllers.MainController{})
	beego.Router("/resend", &controllers.MainController{})
	beego.Router("/signup", &controllers.MainController{})
	beego.Router("/oauth", &controllers.MainController{})
	beego.Router("/icon", &controllers.MainController{})
	beego.Router("/formor", &controllers.MainController{})

	beego.Router("/search", &controllers.MainController{})
	beego.Router("/formor", &controllers.MainController{})
	beego.Router("/create", &controllers.MainController{})
}
